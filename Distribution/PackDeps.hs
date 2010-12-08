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
    ) where

import System.Directory (getAppUserDataDirectory)
import qualified Data.Map as Map
import Data.List (foldl', group, sort, isInfixOf)
import Data.Time (UTCTime (UTCTime), addUTCTime)
import Data.Maybe (mapMaybe)

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import Distribution.Text

import Data.Char (toLower)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString.Lazy as L

import Data.List.Split (splitOn)
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

loadNewest :: IO Newest
loadNewest = do
    c <- getAppUserDataDirectory "cabal"
    let fn = c ++ "/packages/hackage.haskell.org/00-index.tar"
    loadNewestFrom fn

loadNewestFrom :: FilePath -> IO Newest
loadNewestFrom = fmap parseNewest . L.readFile

parseNewest :: L.ByteString -> Newest
parseNewest = foldl' addPackage Map.empty . entriesToList . Tar.read

entriesToList :: Tar.Entries -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = error s
entriesToList (Tar.Next e es) = e : entriesToList es

addPackage :: Newest -> Tar.Entry -> Newest
addPackage m entry =
    case splitOn "/" $ Tar.entryPath entry of
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

-- | The newest version of every package.
type Newest = Map.Map String PackInfo

-- | Information on a single package.
data DescInfo = DescInfo
    { diHaystack :: String
    , diDeps :: [Dependency]
    , diPackage :: PackageIdentifier
    }
    deriving (Show, Read)

getDescInfo :: GenericPackageDescription -> DescInfo
getDescInfo gpd = DescInfo
    { diHaystack = map toLower $ author p ++ maintainer p ++ name
    , diDeps = getDeps gpd
    , diPackage = pi'
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
        Nothing -> Just ((s, " no version found"), 0)
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
        if needle' `isInfixOf` diHaystack desc
            then Just desc
            else Nothing
    go _ = Nothing
