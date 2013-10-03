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
      -- * Helpers
    , diName
      -- * Internal
    , PackInfo (..)
    , DescInfo (..)
    ) where

import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Data.List (foldl', group, sort, isInfixOf, isPrefixOf)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Exception (throw)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Distribution.Text

import Data.Char (toLower, isSpace)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as L

import Data.List.Split (splitOn)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import Data.Function (on)
import Control.Arrow ((&&&))
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set

loadNewest :: IO Newest
loadNewest = do
    c <- getAppUserDataDirectory "cabal"
    cfg <- readFile (c </> "config")
    let repos        = reposFromConfig cfg
        repoCache    = case lookupInConfig "remote-repo-cache" cfg of
            []        -> c </> "packages"  -- Default
            (rrc : _) -> rrc               -- User-specified
        tarName repo = repoCache </> repo </> "00-index.tar"
    fmap (Map.unionsWith maxVersion) . mapM (loadNewestFrom . tarName) $ repos

reposFromConfig :: String -> [String]
reposFromConfig = map (takeWhile (/= ':')) . lookupInConfig "remote-repo"

-- | Looks up the given key in the cabal configuration file
lookupInConfig :: String -> String -> [String]
lookupInConfig key = map trim . catMaybes . map (dropPrefix prefix) . lines
  where
    prefix = key ++ ":"

-- | Utility: drop leading and trailing spaces from a string
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix prefix s =
  if prefix `isPrefixOf` s
  then Just . drop (length prefix) $ s
  else Nothing

loadNewestFrom :: FilePath -> IO Newest
loadNewestFrom = fmap parseNewest . L.readFile

parseNewest :: L.ByteString -> Newest
parseNewest = foldl' addPackage Map.empty . entriesToList . Tar.read

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = throw s
entriesToList (Tar.Next e es) = e : entriesToList es

addPackage :: Newest -> Tar.Entry -> Newest
addPackage m entry =
    case splitOn "/" $ Tar.fromTarPathToPosixPath (Tar.entryTarPath entry) of
        [package', versionS, _] ->
            case simpleParse versionS of
                Just version ->
                    case Map.lookup package' m of
                        Nothing -> go package' version
                        Just PackInfo { piVersion = oldv } ->
                            if version > oldv
                                then go package' version
                                else m
                Nothing -> m
        _ -> m
  where
    go package' version =
        case Tar.entryContent entry of
            Tar.NormalFile bs _ ->
                 Map.insert package' PackInfo
                        { piVersion = version
                        , piDesc = parsePackage bs
                        , piEpoch = Tar.entryTime entry
                        } m
            _ -> m

data PackInfo = PackInfo
    { piVersion :: Version
    , piDesc :: Maybe DescInfo
    , piEpoch :: Tar.EpochTime
    }
    deriving (Show, Read)

maxVersion :: PackInfo -> PackInfo -> PackInfo
maxVersion pi1 pi2 = if piVersion pi1 <= piVersion pi2 then pi2 else pi1

-- | The newest version of every package.
type Newest = Map.Map String PackInfo

type Reverses = Map.Map String (Version, [(String, VersionRange)])

getReverses :: Newest -> Reverses
getReverses newest =
    Map.fromList withVersion
  where
    -- dep = dependency, rel = relying package
    toTuples (_, PackInfo { piDesc = Nothing }) = []
    toTuples (rel, PackInfo { piDesc = Just DescInfo { diDeps = deps } }) =
        map (toTuple rel) deps
    toTuple rel (Dependency (PackageName dep) range) = (dep, (rel, range))
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
    { diHaystack :: String
    , diDeps :: [Dependency]
    , diPackage :: PackageIdentifier
    , diSynopsis :: String
    }
    deriving (Show, Read)

getDescInfo :: GenericPackageDescription -> DescInfo
getDescInfo gpd = DescInfo
    { diHaystack = map toLower $ author p ++ maintainer p ++ name
    , diDeps = getDeps gpd
    , diPackage = pi'
    , diSynopsis = synopsis p
    }
  where
    p = packageDescription gpd
    pi'@(PackageIdentifier (PackageName name) _) = package p

getDeps :: GenericPackageDescription -> [Dependency]
getDeps x = concat
          $ maybe id ((:) . condTreeConstraints) (condLibrary x)
          $ map (condTreeConstraints . snd) (condExecutables x)

checkDeps :: Newest -> DescInfo
          -> (PackageName, Version, CheckDepsRes)
checkDeps newest desc =
    case mapMaybe (notNewest newest) $ diDeps desc of
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
                  | WontAccept [(String, String)] UTCTime
    deriving Show

epochToTime :: Tar.EpochTime -> UTCTime
epochToTime e = addUTCTime (fromIntegral e) $ UTCTime (read "1970-01-01") 0

notNewest :: Newest -> Dependency -> Maybe ((String, String), Tar.EpochTime)
notNewest newest (Dependency (PackageName s) range) =
    case Map.lookup s newest of
        --Nothing -> Just ((s, " no version found"), 0)
        Nothing -> Nothing
        Just PackInfo { piVersion = version, piEpoch = e } ->
            if withinRange version range
                then Nothing
                else Just ((s, display version), e)

-- | Loads up the newest version of a package from the 'Newest' list, if
-- available.
getPackage :: String -> Newest -> Maybe DescInfo
getPackage s n = Map.lookup s n >>= piDesc

-- | Parse information on a package from the contents of a cabal file.
parsePackage :: L.ByteString -> Maybe DescInfo
parsePackage lbs =
    case parsePackageDescription $ T.unpack
       $ T.decodeUtf8With T.lenientDecode lbs of
        ParseOk _ x -> Just $ getDescInfo x
        _ -> Nothing

-- | Load a single package from a cabal file.
loadPackage :: FilePath -> IO (Maybe DescInfo)
loadPackage = fmap parsePackage . L.readFile

-- | Find all of the packages matching a given search string.
filterPackages :: String -> Newest -> [DescInfo]
filterPackages needle =
    mapMaybe go . Map.elems
  where
    needle' = map toLower needle
    go PackInfo { piDesc = Just desc } =
        if needle' `isInfixOf` diHaystack desc &&
           not ("(deprecated)" `isInfixOf` diSynopsis desc)
            then Just desc
            else Nothing
    go _ = Nothing

-- | Find all packages depended upon by the given list of packages.
deepDeps :: Newest -> [DescInfo] -> [DescInfo]
deepDeps newest dis0 =
    go Set.empty dis0
  where
    go _ [] = []
    go viewed (di:dis)
        | name `Set.member` viewed = go viewed dis
        | otherwise = di : go viewed' (newDis ++ dis)
      where
        PackageIdentifier (PackageName name) _ = diPackage di
        viewed' = Set.insert name viewed
        newDis = mapMaybe getDI $ diDeps di
        getDI :: Dependency -> Maybe DescInfo
        getDI (Dependency (PackageName name') _) = do
            pi' <- Map.lookup name' newest
            piDesc pi'

diName :: DescInfo -> String
diName =
    unPN . pkgName . diPackage
  where
    unPN (PackageName pn) = pn
