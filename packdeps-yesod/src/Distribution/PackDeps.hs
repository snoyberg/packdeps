{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.PackDeps
    ( -- * Data types
      Newest (..)
    , CheckDepsRes (..)
    , Reason (..)
    , Outdated (..)
    , DescInfo
      -- * Read package database
    , loadNewest
    , loadNewestFrom
    , parseNewest
      -- * Check a package
    , checkDeps
      -- * Get a single package
    , getPackage
    , parsePackage
    , loadPackage
      -- * Get multiple packages
    , filterPackages
    , deepDeps
      -- * Reverse dependencies
    , Reverses
    , getReverses
      -- * Licenses
    , getLicenseMap
    ) where

import Prelude hiding (pi)
import Data.Text (Text, isInfixOf, toCaseFold, pack)
import qualified Data.HashMap.Strict as HMap

import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Data.Foldable (foldl', foldlM)
import Data.List (isPrefixOf)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Control.Exception (throw)
import Control.Monad ((<=<), when)

import Distribution.PackDeps.Types
import Distribution.PackDeps.Util

import Distribution.Package hiding (PackageName)
import Distribution.License (licenseFromSPDX)
import qualified Distribution.Package as D
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Types.CondTree (CondBranch (..))
import qualified Distribution.Version as D
import Distribution.Text hiding (Text)

import qualified Data.Text as TS
import qualified Data.ByteString.Lazy as L

import Data.List.Split (splitOn)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Control.Arrow (second)
import qualified Data.Set as Set
import Control.Monad.Trans.State (State, evalState, get)
import Control.Lens
import Data.Hashable (Hashable)

loadNewest :: IO Newest
loadNewest = do
    c <- getAppUserDataDirectory "cabal"
    cfg <- readFile (c </> "config")
    let repos        = reposFromConfig cfg
        tarName repo = c </> "packages" </> repo </> "00-index.tar"
    fmap (Newest . unionsWith maxVersion . map unNewest) $ mapM (loadNewestFrom . tarName) repos

unionsWith :: (Foldable f, Hashable k, Eq k)
           => (v -> v -> v)
           -> f (HMap.HashMap k v)
           -> HMap.HashMap k v
unionsWith f = foldl' (HMap.unionWith f) HMap.empty

reposFromConfig :: String -> [String]
reposFromConfig = map (takeWhile (/= ':'))
                . catMaybes
                . map (dropPrefix "remote-repo: ")
                . lines

dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix prefix s =
  if prefix `isPrefixOf` s
  then Just . drop (length prefix) $ s
  else Nothing

loadNewestFrom :: FilePath -> IO Newest
loadNewestFrom = parseNewest <=< L.readFile

parseNewest :: L.ByteString -> IO Newest
parseNewest = fmap fst . foldlM addPackage (Newest HMap.empty, 0) . entriesToList . Tar.read

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = throw s
entriesToList (Tar.Next e es) = e : entriesToList es

addPackage :: (Newest, Int) -> Tar.Entry -> IO (Newest, Int)
addPackage (Newest m, count) entry = do
    when (count' > 0 && count' `mod` 100 == 0) $ putStrLn $ "Processed cabal file count: " ++ show count'
    return (Newest m', count')
  where
    (m', count') =
      case splitOn "/" $ Tar.fromTarPathToPosixPath (Tar.entryTarPath entry) of
        [package', versionS, name] | ".cabal" `TS.isSuffixOf` TS.pack name ->
            let package'' = PackageName $ D.mkPackageName package'
             in case fmap convertVersion $ simpleParse versionS of
                    _ | package' == "acme-everything" -> (m, count) -- takes too long to parse
                    Just version ->
                        case HMap.lookup package'' m of
                            Nothing -> go package'' version
                            Just PackInfo { piVersion = oldv } ->
                                if version >= oldv
                                    then go package'' version
                                    else (m, count)
                    Nothing -> (m, count)
        _ -> (m, count)

    go package' version =
        case Tar.entryContent entry of
            Tar.NormalFile bs _ ->
                 let p = parsePackage bs
                  in (HMap.insert package' PackInfo
                        { piVersion = version
                        , piDesc = fmap fst p
                        , piEpoch = Tar.entryTime entry
                        , piLicense =
                            case p of
                                Nothing' -> License "Unknown"
                                Just' (_, l) -> l
                        } m, count + 1)
            _ -> (m, count)

maxVersion :: Ord v => PackInfo n v l -> PackInfo n v l -> PackInfo n v l
maxVersion pi1 pi2 = if piVersion pi1 <= piVersion pi2 then pi2 else pi1

getReverses :: Newest -> Reverses
getReverses (Newest newest) =
    HMap.fromList withVersion
  where
    -- dep = dependency, rel = relying package
    toTuples :: (PackageName, PackInfo PackageName version license)
             -> HMap.HashMap PackageName (HMap.HashMap PackageName (VersionRange version, Text))
    toTuples (_, PackInfo { piDesc = Nothing' }) = HMap.empty
    toTuples (rel, PackInfo { piDesc = Just' desc@DescInfo { diDeps = deps } })
        | isDeprecated desc = HMap.empty
        | otherwise = combine $ map (toTuple rel) $ HMap.toList deps

    combine = unionsWith HMap.union

    toTuple rel (dep, ((PUVersionRange _ range), syn)) =
        if rel == dep
            then HMap.empty
            else HMap.singleton dep $ HMap.singleton rel (range, syn)

    hoisted :: HMap.HashMap PackageName (HMap.HashMap PackageName (VersionRange Version, Text))
    hoisted = combine $ map toTuples $ HMap.toList newest

    withVersion = mapMaybe addExtraInfo $ HMap.toList hoisted

    addExtraInfo (dep, rels) =
        case HMap.lookup dep newest of
            Nothing -> Nothing
            Just PackInfo { piVersion = v, piDesc = Just' DescInfo { diSynopsis = syn } }
              -> Just (dep, ((v, syn), rels))
            Just PackInfo { piVersion = v, piDesc = Nothing' }
              -> Just (dep, ((v, "n/a"), rels))

getDescInfo :: GenericPackageDescription -> (DescInfo PackageName Version, License)
getDescInfo gpd = (DescInfo
    { diHaystack = toCaseFold $ pack $ unlines [author p, maintainer p, name]
    , diDeps = getDeps gpd
    , diSynopsis = pack $ synopsis p
    }, License $ pack $ display $ licenseFromSPDX $ license $ packageDescription gpd)
  where
    p = packageDescription gpd
    PackageIdentifier (D.unPackageName -> name) _version = package p

getDeps :: GenericPackageDescription -> HMap.HashMap PackageName (PUVersionRange (VersionRange Version), Text)
getDeps gpd = HMap.map (\val -> (val, pack $ synopsis $ packageDescription gpd))
            $ HMap.map (fmap convertVersionRange)
            $ foldr (HMap.unionWith mappend) HMap.empty
            $ map (\(Dependency k v, pu) -> HMap.singleton (PackageName k) (PUVersionRange pu v))
            $ mconcat
                [ maybe mempty (map (, Runtime) . setupDepends)
                    $ setupBuildInfo $ packageDescription gpd
                , maybe mempty (go Runtime) $ condLibrary gpd
                , mconcat $ map (go Runtime . snd) $ condSubLibraries gpd
                , mconcat $ map (go Runtime . snd) $ condForeignLibs gpd
                , mconcat $ map (go Runtime . snd) $ condExecutables gpd
                , mconcat $ map (go TestBench . snd) $ condTestSuites gpd
                , mconcat $ map (go TestBench . snd) $ condBenchmarks gpd
                ]
  where
    flagMaps =
        take 10 $ -- arbitrary hack to make this thing complete in reasonable time/memory
        loop $ genPackageFlags gpd
      where
        loop [] = return Map.empty
        loop (f:fs) = do
            let name = flagName f
                def = flagDefault f
            rest <- loop fs
            [Map.insert name def rest, Map.insert name (not def) rest]

    go :: PackageUsage -> CondTree ConfVar [Dependency] a -> [(Dependency, PackageUsage)]
    go pu tree = map (, pu) $
        case filter allowsNewBase choices of
            [] ->
                case choices of
                    [] -> []
                    c:_ -> c
            c:_ -> c
      where
        choices = map (flip go' tree) flagMaps

    allowsNewBase :: [Dependency] -> Bool
    allowsNewBase =
        all ok
      where
        Just newbase = simpleParse "4.10.0.0"

        ok :: Dependency -> Bool
        ok (Dependency (D.unPackageName -> "base") range) = newbase `D.withinRange` range
        ok _ = True

    go' flagMap tree
        = concat
        $ condTreeConstraints tree
        : map (go' flagMap) (mapMaybe (checkCond flagMap) $ condTreeComponents tree)

    checkCond flagMap (CondBranch cond tree melse)
        | checkCond' flagMap cond = Just tree
        | otherwise = melse

    checkCond' _ (Var (OS _)) = True
    checkCond' _ (Var (Arch _)) = True
    checkCond' flagMap (Var (Flag f)) = fromMaybe False $ Map.lookup f flagMap
    checkCond' _ (Var (Impl _compiler _range)) = True
    checkCond' _ (Lit b) = b
    checkCond' flagMap (CNot c) = not $ checkCond' flagMap c
    checkCond' flagMap (COr c1 c2) = checkCond' flagMap c1 || checkCond' flagMap c2
    checkCond' flagMap (CAnd c1 c2) = checkCond' flagMap c1 && checkCond' flagMap c2

checkDeps :: Newest
          -> (PackageName, Version, DescInfo PackageName Version)
          -> (PackageName, Version, CheckDepsRes)
checkDeps newest (name, version, desc) =
    case mapMaybe (notNewest newest) $ HMap.toList $ diDeps desc of
        [] -> (name, version, AllNewest)
        x -> let y = HMap.fromList $ map fst x
                 et = maximum $ map snd x
              in (name, version, WontAccept y $ epochToTime et)

-- | Whether or not a package can accept all of the newest versions of its
-- dependencies. If not, it returns a list of packages which are not accepted,
-- and a timestamp of the most recently updated package.
data CheckDepsRes = AllNewest
                  | WontAccept (HMap.HashMap PackageName Outdated) UTCTime
    deriving Show

data Outdated = Outdated Version Reason

instance Show Outdated where
    show (Outdated _ Deprecated) = "deprecated"
    show (Outdated version NewerAvailable) = show version
    show (Outdated version NewerAndDeprecated) = show version ++ " (deprecated)"

data Reason = NewerAvailable | Deprecated | NewerAndDeprecated
    deriving Show

epochToTime :: Tar.EpochTime -> UTCTime
epochToTime e = addUTCTime (fromIntegral e) $ UTCTime (read "1970-01-01") 0

notNewest :: Newest
          -> (PackageName, (PUVersionRange (VersionRange Version), Text))
          -> Maybe ((PackageName, Outdated), Tar.EpochTime)
notNewest (Newest newest) (s, (PUVersionRange _ range, _)) =
    case HMap.lookup s newest of
        --Nothing -> Just ((s, " no version found"), 0)
        Nothing -> Nothing
        Just PackInfo { piVersion = version, piEpoch = e, piDesc = d } ->
            let mreason =
                    case (maybe' False isDeprecated d, not $ withinRange version range) of
                        (False, False) -> Nothing
                        (True, False) -> Just Deprecated
                        (False, True) -> Just NewerAvailable
                        (True, True) -> Just NewerAndDeprecated
             in flip fmap mreason $ \reason -> ((s, Outdated version reason), e)

-- | Loads up the newest version of a package from the 'Newest' list, if
-- available.
getPackage :: PackageName -> Newest -> Maybe (PackageName, Version, DescInfo PackageName Version)
getPackage s (Newest n) = do
    pi <- HMap.lookup s n
    di <- m'ToM $ piDesc pi
    return (s, piVersion pi, di)

-- | Parse information on a package from the contents of a cabal file.
parsePackage :: L.ByteString -> Maybe' (DescInfo PackageName Version, License)
parsePackage lbs =
    case snd $ runParseResult $ parseGenericPackageDescription $ L.toStrict lbs of
        Right x -> Just' $ getDescInfo x
        Left _ -> Nothing'

-- | Load a single package from a cabal file.
loadPackage :: FilePath -> IO (Maybe' (DescInfo PackageName Version))
loadPackage = fmap (fmap fst . parsePackage) . L.readFile

isDeprecated :: DescInfo name version -> Bool
isDeprecated desc = "(deprecated)" `isInfixOf` diSynopsis desc

-- | Find all of the packages matching a given search string.
filterPackages :: Text -> Newest -> [(PackageName, Version, DescInfo PackageName Version)]
filterPackages needle =
    mapMaybe go . HMap.toList . unNewest
  where
    go (name, PackInfo { piVersion = v, piDesc = Just' desc }) =
        if matches (diHaystack desc) &&
           not (isDeprecated desc)
            then Just (name, v, desc)
            else Nothing
    go _ = Nothing

    matches haystack
        | Just needle' <- TS.stripPrefix "exact:" needle = all (`elem` TS.words haystack) $ TS.words $ toCaseFold needle'
        | otherwise =
            let (needle', excludes) = splitExcludes $ toCaseFold needle
             in (needle' `isInfixOf` haystack) && all (\t -> not $ t `isInfixOf` haystack) excludes

    splitExcludes = second (filter (not . TS.null) . TS.split (== '!'))
                  . TS.break (== '!')

-- | Find all packages depended upon by the given list of packages.
deepDeps :: Newest
         -> [(PackageName, Version, DescInfo PackageName Version)]
         -> [(PackageName, Version, DescInfo PackageName Version)]
deepDeps (Newest newest) dis0 =
    go Set.empty dis0
  where
    go _ [] = []
    go viewed ((name, v, di):dis)
        | name `Set.member` viewed = go viewed dis
        | otherwise = (name, v, di) : go viewed' (newDis ++ dis)
      where
        viewed' = Set.insert name viewed
        newDis = mapMaybe getDI $ HMap.keys $ diDeps di
        getDI name' = do
            pi <- HMap.lookup name' newest
            di' <- m'ToM $ piDesc pi
            return (name', piVersion pi, di')

data LMS = LMS
    { _lmsProcessed :: Set.Set PackageName
    , _lmsToProcess :: [PackageName]
    , _lmsResult :: LicenseMap
    }
makeLenses ''LMS

getLicenseMap :: Bool -- ^ include test/benchmarks
              -> Newest -> LicenseMap
getLicenseMap includeTests (Newest newest) =
    evalState go (LMS Set.empty (HMap.keys newest) Map.empty)
  where
    go = do
        lms <- get
        case lms ^. lmsToProcess of
            [] -> return $ lms ^. lmsResult
            p:rest -> do
                lmsToProcess %= const rest
                _ <- getLicenses p
                go

    getLicenses :: PackageName -> State LMS Licenses
    getLicenses p = do
        lms1 <- get
        if p `Set.member` (lms1 ^. lmsProcessed)
            then return $ fromMaybe mempty $ Map.lookup p $ lms1 ^. lmsResult
            else do
                lmsProcessed %= Set.insert p
                case HMap.lookup p newest of
                    Nothing -> return mempty
                    Just pi -> do
                        let ls1 = Licenses $ Map.singleton (piLicense pi) $ Set.singleton p
                            deps =
                                case piDesc pi of
                                    Nothing' -> []
                                    Just' di -> map fst $ filter isIncluded $ HMap.toList $ diDeps di
                        lss <- mapM getLicenses deps
                        let ls = mconcat $ ls1 : lss
                        lmsResult %= Map.insert p ls
                        return ls

    isIncluded (_, (PUVersionRange Runtime _, _)) = True
    isIncluded (_, (PUVersionRange TestBench _, _)) = includeTests
