{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Distribution.PackDeps
    ( -- * Data types
      Newest
    , CheckDepsRes (..)
    , DescInfo
      -- * Read package database
    , loadNewest
    , loadNewestFrom
    , parseNewest
      -- * Check a package
    , checkDeps
    , checkLibDeps
      -- * Get a single package
    , getPackage
    , parsePackage
    , loadPackage
      -- * Get multiple packages
    , filterPackages
    , deepDeps
    , deepLibDeps
      -- * Reverse dependencies
    , Reverses
    , getReverses
      -- * Helpers
    , diName
      -- * Internal
    , PackInfo (..)
    , piRevision
    , DescInfo (..)
    ) where

import Control.Applicative as A ((<$>))
import Control.Monad (guard)
import System.Directory (getAppUserDataDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Map.Strict as Map
import Data.List (foldl', group, sort, isInfixOf)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Exception (throw)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Parsec (Parsec, lexemeParsec, runParsecParser, simpleParsec)
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromBS)
import Distribution.Types.CondTree
import Distribution.Version
import Distribution.Utils.ShortText
import qualified Distribution.Utils.ShortText as ShortText
import qualified Distribution.Fields as F
import qualified Distribution.Fields.Field as F
import qualified Distribution.Simple.Utils as U

import Data.Char (toLower)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Data.List.Split (splitOn)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Data.Function (on)
import Control.Arrow ((&&&))
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Text.Read (readMaybe)
import System.Environment (lookupEnv)
-- import Data.Monoid ((<>))

loadNewest :: Bool -> IO Newest
loadNewest preferred = do
    c <- getAppUserDataDirectory "cabal"
    location <- fmap (fromMaybe (c </> "config")) (lookupEnv "CABAL_CONFIG")

    cfg' <- S.readFile location
    cfg <- either (fail . show) return $ F.readFields cfg'
    let repos        = reposFromConfig cfg
        repoCache    = case lookupInConfig "remote-repo-cache" cfg of
            []        -> c </> "packages"  -- Default
            (rrc : _) -> rrc               -- User-specified
        tarNames repo = [ pfx </> "01-index.tar", pfx </> "00-index.tar" ]
          where pfx = repoCache </> repo
    fmap (Map.unionsWith maxVersion) . mapM (loadNewestFrom preferred . tarNames) $ repos

-- | Takes a list of possible pathes, tries them in order until one exists.
loadNewestFrom :: Bool -> [FilePath] -> IO Newest
loadNewestFrom _ []         = fail "loadNewestFrom: no index tarball"
loadNewestFrom preferred (fp : fps) = do
    e <- doesFileExist fp
    if e
        then fmap (parseNewest preferred) (L.readFile fp)
        else loadNewestFrom preferred fps

parseNewest :: Bool -> L.ByteString -> Newest
parseNewest preferred lbs =
    foldl' (addPackage pref) Map.empty . entriesToList . Tar.read $ lbs
  where
    pref | preferred = parsePreferred lbs
         | otherwise = Map.empty

parsePreferred :: L.ByteString -> Preferred
parsePreferred = foldl' addPreferred Map.empty . entriesToList . Tar.read

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = throw s
entriesToList (Tar.Next e es) = e : entriesToList es

addPackage :: Preferred -> Newest -> Tar.Entry -> Newest
addPackage pref m entry =
    case splitOn "/" $ Tar.fromTarPathToPosixPath (Tar.entryTarPath entry) of
        [packageS, versionS, _] -> fromMaybe m $ do
            package' <- simpleParsec packageS
            version <- simpleParsec versionS

            -- if there are preferred versions, consider only them
            guard (maybe True (withinRange version) $ Map.lookup package' pref)

            case Map.lookup package' m of
                Nothing -> return $ go package' version
                Just PackInfo { piVersion = oldv } -> do
                    -- in 01-index.tar there are entries with the same versions
                    guard (version >= oldv)
                    return $ go package' version
        _ -> m -- error (show entry)
  where
    go package' version =
        case Tar.entryContent entry of
            Tar.NormalFile lbs _ ->
                let bs = S.copy (L.toStrict lbs)
                in bs `seq` Map.insertWith maxVersion package' PackInfo
                        { piVersion = version
                        , piDesc = parsePackage bs
                        , piEpoch = Tar.entryTime entry
                        } m
            _ -> m

addPreferred :: Preferred -> Tar.Entry -> Preferred
addPreferred m entry =
    case splitOn "/" $ Tar.fromTarPathToPosixPath (Tar.entryTarPath entry) of
        [_packageS, "preferred-versions"] ->
            case Tar.entryContent entry of
                Tar.NormalFile bs _ -> case simpleParsecLBS bs of
                    Just (Dependency dep range _) -> Map.insert dep range m
                    Nothing -> m
                _ -> m
        _ -> m

simpleParsecLBS :: Parsec a => L.ByteString -> Maybe a
simpleParsecLBS = either (const Nothing) Just
    . runParsecParser lexemeParsec "<simpleParsec>"
    . fieldLineStreamFromBS
    . S.copy
    . L.toStrict

data PackInfo = PackInfo
    { piVersion  :: !Version
    , piDesc     :: Maybe DescInfo -- this is deliberately lazy
    , piEpoch    :: !Tar.EpochTime
    }
    deriving (Show, Read)

-- Extract revision from PackInfo, default to 0
piRevision :: PackInfo -> Int
piRevision = fromMaybe 0 . fmap diRevision . piDesc

-- We should compare revisions as well,
-- but we don't do that, as it would force `piDesc` and make `packdeps`
-- executable increadibly slow (parse all cabal files on Hackage)
-- Instead we rely on the fact that newer revisions are always later in
-- 01-index.tar
maxVersion :: PackInfo -> PackInfo -> PackInfo
maxVersion pi1 pi2 = if piVersion pi1 <= piVersion pi2 then pi2 else pi1

-- | The newest version of every package.
type Newest = Map.Map PackageName PackInfo

-- | The preferred versions of every package
type Preferred = Map.Map PackageName VersionRange

type Reverses = Map.Map PackageName (Version, [(PackageName, VersionRange)])

getReverses :: Newest -> Reverses
getReverses newest =
    Map.fromList withVersion
  where
    -- dep = dependency, rel = relying package
    toTuples (_, PackInfo { piDesc = Nothing }) = []
    toTuples (rel, PackInfo { piDesc = Just DescInfo { diDeps = deps } }) =
        map (toTuple rel) deps
    toTuple rel (Dependency dep range _) = (dep, (rel, range))
    hoist :: Ord a => [(a, b)] -> [(a, [b])]
    hoist = map ((fst . head) &&& map snd)
          . groupBy ((==) `on` fst)
          . sortBy (comparing fst)
    hoisted = hoist $ concatMap toTuples $ Map.toList newest
    withVersion = mapMaybe addVersion hoisted
    addVersion (dep, rels) =
        case Map.lookup dep newest of
            Nothing -> Nothing
            Just PackInfo { piVersion = v} -> Just (dep, (v, rels))

-- | Information on a single package.
data DescInfo = DescInfo
    { diHaystack :: ShortText
    , diDeps     :: [Dependency]
    , diLibDeps  :: [Dependency]
    , diPackage  :: PackageIdentifier
    , diRevision :: Int
    , diSynopsis :: ShortText
    }
    deriving (Show, Read)

-- | Return revision and DescInfo
getDescInfo :: GenericPackageDescription -> DescInfo
getDescInfo gpd = DescInfo
    { diHaystack = ShortText.toShortText $
                   map toLower $
                   ShortText.fromShortText $
                   author p <> maintainer p <> ShortText.toShortText name
    , diDeps     = getDeps gpd
    , diLibDeps  = getLibDeps gpd
    , diPackage  = pi'
    , diRevision = rev
    , diSynopsis = synopsis p
    }
  where
    p = packageDescription gpd
    pi'@(PackageIdentifier (unPackageName -> name) _) = package p
    rev = fromMaybe 0 $ do
        r <- lookup "x-revision" (customFieldsPD p)
        readMaybe r

getDeps :: GenericPackageDescription -> [Dependency]
getDeps x = getLibDeps x ++ concat
    [ concatMap (condTreeConstraints' . snd) (condExecutables x)
    , concatMap (condTreeConstraints' . snd) (condTestSuites x)
    , concatMap (condTreeConstraints' . snd) (condBenchmarks x)
    ]

getLibDeps :: GenericPackageDescription -> [Dependency]
getLibDeps gpd = maybe [] condTreeConstraints' (condLibrary gpd) ++ customDeps
  where
    pd = packageDescription gpd
    customDeps
        | buildType pd == Custom = maybe defSetupDeps setupDepends (setupBuildInfo pd)
        | otherwise = []

    -- we only interested in Cabal default upper bound
    -- See cabal-install Distribution.Client.ProjectPlanning defaultSetupDeps
    defSetupDeps = [Dependency (mkPackageName "Cabal") (earlierVersion $ mkVersion [1,25])
#if MIN_VERSION_Cabal(3, 4, 0)
                               mainLibSet
#else
                               mempty
#endif
                   ]

condTreeConstraints' :: Monoid c => CondTree ConfVar c a  -> c
condTreeConstraints' = go where
    go (CondNode _ c bs) = mappend c (foldMap branch bs)

    branch (CondBranch c' x y)
        | notFlagCondition c' = mappend (go x) (foldMap go y)
        | otherwise           = mempty

notFlagCondition :: Condition ConfVar -> Bool
notFlagCondition (Var (OS _))     = True
notFlagCondition (Var (Arch _))   = True
#if MIN_VERSION_Cabal(3, 4, 0)
notFlagCondition (Var (PackageFlag _)) = False
#else
notFlagCondition (Var (Flag _))   = False
#endif
notFlagCondition (Var (Impl _ _)) = True
notFlagCondition (Lit _)          = True
notFlagCondition (CNot a)         = notFlagCondition a
notFlagCondition (COr a b)        = notFlagCondition a && notFlagCondition b
notFlagCondition (CAnd a b)       = notFlagCondition a && notFlagCondition b

checkDeps :: Newest -> DescInfo
          -> (PackageName, Version, CheckDepsRes)
checkDeps = checkDepsImpl diDeps

checkLibDeps :: Newest -> DescInfo
             -> (PackageName, Version, CheckDepsRes)
checkLibDeps = checkDepsImpl diLibDeps

checkDepsImpl :: (DescInfo -> [Dependency]) -> Newest -> DescInfo
              -> (PackageName, Version, CheckDepsRes)
checkDepsImpl deps newest desc =
    case mapMaybe (notNewest newest) $ deps desc of
        [] -> (name, version, AllNewest)
        x -> let y = map head $ group $ sort $ map fst x
                 et = maximum $ map snd x
              in (name, version, WontAccept y $ epochToTime et)
  where
    PackageIdentifier name version = diPackage desc

-- | Whether or not a package can accept all of the newest versions of its
-- dependencies. If not, it returns a list of packages which are not accepted,
-- and a timestamp of the most recently updated package.
data CheckDepsRes = AllNewest
                  | WontAccept [(PackageName, Version)] UTCTime
    deriving Show

epochToTime :: Tar.EpochTime -> UTCTime
epochToTime e = addUTCTime (fromIntegral e) $ UTCTime (read "1970-01-01") 0

notNewest :: Newest -> Dependency -> Maybe ((PackageName, Version), Tar.EpochTime)
notNewest newest (Dependency s range _public) =
    case Map.lookup s newest of
        --Nothing -> Just ((s, " no version found"), 0)
        Nothing -> Nothing
        Just PackInfo { piVersion = version, piEpoch = e } ->
            if withinRange version range
                then Nothing
                else Just ((s, version), e)

-- | Loads up the newest version of a package from the 'Newest' list, if
-- available.
getPackage :: PackageName -> Newest -> Maybe DescInfo
getPackage s n = Map.lookup s n >>= piDesc

-- | Parse information on a package from the contents of a cabal file.
parsePackage :: S.ByteString -> Maybe DescInfo
parsePackage bs =
    case snd $ runParseResult $ parseGenericPackageDescription bs of
        Right x -> Just $ getDescInfo x
        Left _  -> Nothing

-- | Load a single package from a cabal file.
loadPackage :: FilePath -> IO (Maybe DescInfo)
loadPackage = fmap parsePackage . S.readFile

-- | Find all of the packages matching a given search string.
filterPackages :: String -> Newest -> [DescInfo]
filterPackages needle =
    mapMaybe go . Map.elems
  where
    needle' = map toLower needle
    go PackInfo { piDesc = Just desc } =
        if needle' `isInfixOf` ShortText.fromShortText (diHaystack desc) &&
           not ("(deprecated)" `isInfixOf` ShortText.fromShortText (diSynopsis desc))
            then Just desc
            else Nothing
    go _ = Nothing

-- | Find all packages depended upon by the given list of packages.
deepDeps :: Newest -> [DescInfo] -> [DescInfo]
deepDeps = deepDepsImpl diDeps

-- | Find all packages depended upon by the given list of packages.
deepLibDeps :: Newest -> [DescInfo] -> [DescInfo]
deepLibDeps = deepDepsImpl diLibDeps

deepDepsImpl :: (DescInfo -> [Dependency]) -> Newest -> [DescInfo] -> [DescInfo]
deepDepsImpl deps newest dis0 =
    go Set.empty dis0
  where
    go _ [] = []
    go viewed (di:dis)
        | name `Set.member` viewed = go viewed dis
        | otherwise = di : go viewed' (newDis ++ dis)
      where
        PackageIdentifier name _ = diPackage di
        viewed' = Set.insert name viewed
        newDis = mapMaybe getDI $ deps di
        getDI :: Dependency -> Maybe DescInfo
        getDI (Dependency name' _ _) = do
            pi' <- Map.lookup name' newest
            piDesc pi'

diName :: DescInfo -> String
diName = unPackageName . pkgName . diPackage

-------------------------------------------------------------------------------
-- ~/.cabal/config parsing
-------------------------------------------------------------------------------

reposFromConfig :: [F.Field ann] -> [String]
reposFromConfig fields = takeWhile (/= ':') A.<$> mapMaybe f fields
  where
    f (F.Field (F.Name _pos name) fls)
        | name == "remote-repo"
        = Just $ U.fromUTF8BS $ S.intercalate "\n" (map F.fieldLineBS fls)
    f (F.Section (F.Name _pos secName) (F.SecArgName _ arg : _) _fields)
        | secName == "repository"
        = Just $ U.fromUTF8BS arg
    f _ = Nothing

-- | Looks up the given key in the cabal configuration file
lookupInConfig :: S.ByteString -> [F.Field ann] -> [String]
lookupInConfig key = mapMaybe f
  where
    f (F.Field (F.Name _pos name) fls)
        | name == key
        = Just $ U.fromUTF8BS $ S.intercalate "\n" (map F.fieldLineBS fls)
    f _ = Nothing

{-
-- | Like 'either', but for 'ParseResult'
parseResult :: (F.PError -> r) -> (a -> r) -> F.ParseResult a -> r
parseResult l _ (F.ParseFailed e)    = l e
parseResult _ r (F.ParseOk _warns x) = r x
-}
