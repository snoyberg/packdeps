{-# LANGUAGE OverloadedStrings #-}
module Distribution.PackDeps
    ( -- * Data types
      Newest (..)
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
    ) where

import Prelude
import Data.Text (Text, isInfixOf, toCaseFold, pack)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector as Vector

import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))
import qualified Data.Map as Map
import Data.List (foldl', group, sort, isPrefixOf)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Maybe (mapMaybe, catMaybes)
import Control.Exception (throw)
import Control.Monad (join)

import Distribution.PackDeps.Types
import Distribution.PackDeps.Util

import Distribution.Package hiding (PackageName)
import qualified Distribution.Package as D
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import qualified Distribution.Version as D
import Distribution.Text hiding (Text)

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
        tarName repo = c </> "packages" </> repo </> "00-index.tar"
    fmap (Newest . unionsWith maxVersion . map unNewest) $ mapM (loadNewestFrom . tarName) repos

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
loadNewestFrom = fmap parseNewest . L.readFile

parseNewest :: L.ByteString -> Newest
parseNewest = foldl' addPackage (Newest HMap.empty) . entriesToList . Tar.read

entriesToList :: Tar.Entries Tar.FormatError -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = throw s
entriesToList (Tar.Next e es) = e : entriesToList es

addPackage :: Newest -> Tar.Entry -> Newest
addPackage (Newest m) entry = Newest $
    case splitOn "/" $ Tar.fromTarPathToPosixPath (Tar.entryTarPath entry) of
        [package', versionS, _] ->
            let package'' = PackageName $ pack package'
             in case fmap convertVersion $ simpleParse versionS of
                    Just version ->
                        case HMap.lookup package'' m of
                            Nothing -> go package'' version
                            Just PackInfo { piVersion = oldv } ->
                                if version > oldv
                                    then go package'' version
                                    else m
                    Nothing -> m
        _ -> m
  where
    go package' version =
        case Tar.entryContent entry of
            Tar.NormalFile bs _ ->
                 HMap.insert package' PackInfo
                        { piVersion = version
                        , piDesc = parsePackage bs
                        , piEpoch = Tar.entryTime entry
                        } m
            _ -> m

maxVersion :: Ord v => PackInfo n v -> PackInfo n v -> PackInfo n v
maxVersion pi1 pi2 = if piVersion pi1 <= piVersion pi2 then pi2 else pi1

getReverses :: Newest -> Reverses
getReverses (Newest newest) =
    HMap.fromList withVersion
  where
    -- dep = dependency, rel = relying package
    --toTuples :: (PackageName, PackInfo) -> HMap.HashMap PackageName (HMap.HashMap PackageName VersionRange)
    toTuples (_, PackInfo { piDesc = Nothing' }) = HMap.empty
    toTuples (rel, PackInfo { piDesc = Just' DescInfo { diDeps = deps } }) =
        combine $ map (toTuple rel) $ HMap.toList deps

    combine = unionsWith HMap.union

    toTuple rel (dep, range) = HMap.singleton dep $ HMap.singleton rel range

    hoisted :: HMap.HashMap PackageName (HMap.HashMap PackageName (VersionRange Version))
    hoisted = combine $ map toTuples $ HMap.toList newest

    withVersion = mapMaybe addVersion $ HMap.toList hoisted

    addVersion (dep, rels) =
        case HMap.lookup dep newest of
            Nothing -> Nothing
            Just PackInfo { piVersion = v} -> Just (dep, (v, rels))

getDescInfo :: GenericPackageDescription -> DescInfo PackageName Version
getDescInfo gpd = DescInfo
    { diHaystack = toCaseFold $ pack $ author p ++ maintainer p ++ name
    , diDeps = getDeps gpd
    , diSynopsis = pack $ synopsis p
    }
  where
    p = packageDescription gpd
    pi'@(PackageIdentifier (D.PackageName name) version) = package p

getDeps :: GenericPackageDescription -> HMap.HashMap PackageName (VersionRange Version)
getDeps x = HMap.fromList
          $ map (\(Dependency (D.PackageName k) v) -> (PackageName $ pack k, convertVersionRange v))
          $ concat
          $ maybe id ((:) . condTreeConstraints) (condLibrary x)
          $ map (condTreeConstraints . snd) (condExecutables x) ++
            map (condTreeConstraints . snd) (condTestSuites x) ++
            map (condTreeConstraints . snd) (condBenchmarks x)

convertVersionRange :: D.VersionRange -> VersionRange Version
convertVersionRange =
    goR
  where
    goR D.AnyVersion = AnyVersion
    goR (D.ThisVersion x) = ThisVersion $ convertVersion x
    goR (D.LaterVersion x) = LaterVersion $ convertVersion x
    goR (D.EarlierVersion x) = EarlierVersion $ convertVersion x
    goR (D.WildcardVersion x) = WildcardVersion $ convertVersion x
    goR (D.UnionVersionRanges x y) = UnionVersionRanges (goR x) (goR y)
    goR (D.IntersectVersionRanges x y) = IntersectVersionRanges (goR x) (goR y)
    goR (D.VersionRangeParens x) = VersionRangeParens $ goR x

convertVersion :: D.Version -> Version
convertVersion (D.Version x y) = Version (Vector.fromList x) (Vector.fromList $ map pack y)

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
                  | WontAccept (HMap.HashMap PackageName Version) UTCTime
    deriving Show

epochToTime :: Tar.EpochTime -> UTCTime
epochToTime e = addUTCTime (fromIntegral e) $ UTCTime (read "1970-01-01") 0

notNewest :: Newest -> (PackageName, VersionRange Version) -> Maybe ((PackageName, Version), Tar.EpochTime)
notNewest (Newest newest) (s, range) =
    case HMap.lookup s newest of
        --Nothing -> Just ((s, " no version found"), 0)
        Nothing -> Nothing
        Just PackInfo { piVersion = version, piEpoch = e } ->
            if withinRange version range
                then Nothing
                else Just ((s, version), e)

-- | Loads up the newest version of a package from the 'Newest' list, if
-- available.
getPackage :: PackageName -> Newest -> Maybe (PackageName, Version, DescInfo PackageName Version)
getPackage s (Newest n) = do
    pi <- HMap.lookup s n
    di <- m'ToM $ piDesc pi
    return (s, piVersion pi, di)

-- | Parse information on a package from the contents of a cabal file.
parsePackage :: L.ByteString -> Maybe' (DescInfo PackageName Version)
parsePackage lbs =
    case parsePackageDescription $ T.unpack
       $ T.decodeUtf8With T.lenientDecode lbs of
        ParseOk _ x -> Just' $ getDescInfo x
        _ -> Nothing'

-- | Load a single package from a cabal file.
loadPackage :: FilePath -> IO (Maybe' (DescInfo PackageName Version))
loadPackage = fmap parsePackage . L.readFile

-- | Find all of the packages matching a given search string.
filterPackages :: Text -> Newest -> [(PackageName, Version, DescInfo PackageName Version)]
filterPackages needle =
    mapMaybe go . HMap.toList . unNewest
  where
    go (name, PackInfo { piVersion = v, piDesc = Just' desc }) =
        if toCaseFold needle `isInfixOf` diHaystack desc &&
           not ("(deprecated)" `isInfixOf` diSynopsis desc)
            then Just (name, v, desc)
            else Nothing
    go _ = Nothing

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
        getDI name = do
            pi <- HMap.lookup name newest
            di <- m'ToM $ piDesc pi
            return (name, piVersion pi, di)
