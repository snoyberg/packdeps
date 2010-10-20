module CheckDeps
    ( Newest
    , loadNewest
    , filterPackages
    , CheckDeps (..)
    , checkDeps
    ) where

import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.List (foldl', group, sort, isInfixOf)
import Control.Applicative
import Data.List.Split (splitOn)
import Distribution.Text
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import Data.Time

loadNewest :: FilePath -> IO Newest
loadNewest db = foldl' addPackage Map.empty
              . entriesToList . Tar.read <$> L.readFile db

filterPackages :: String -> Newest -> [GenericPackageDescription]
filterPackages needle =
    mapMaybe go . Map.elems
  where
    needle' = map toLower needle
    go PackInfo { piDesc = Just desc } =
        let d = packageDescription desc
            a = author d
            m = maintainer d
            n = show $ package d
            haystack = map toLower $ a ++ m ++ n
         in if needle' `isInfixOf` haystack
                then Just desc
                else Nothing
    go _ = Nothing

data CheckDeps = AllNewest
               | WontAccept [(String, String)] UTCTime
    deriving Show

checkDeps :: Newest -> GenericPackageDescription -> (PackageName, CheckDeps)
checkDeps newest desc =
    case mapMaybe (notNewest newest) $ getDeps desc of
        [] -> (name, AllNewest)
        x -> let y = map head $ group $ sort $ map fst x
                 et = maximum $ map snd x
              in (name, WontAccept y $ epochToTime et)
  where
    PackageIdentifier name _ = package $ packageDescription desc

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

entriesToList :: Tar.Entries -> [Tar.Entry]
entriesToList Tar.Done = []
entriesToList (Tar.Fail s) = error s
entriesToList (Tar.Next e es) = e : entriesToList es

data PackInfo = PackInfo
    { piVersion :: Version
    , piDesc :: Maybe GenericPackageDescription
    , piEpoch :: Tar.EpochTime
    }
type Newest = Map.Map String PackInfo

addPackage :: Newest -> Tar.Entry -> Newest
addPackage m entry =
    case splitOn "/" $ Tar.entryPath entry of
        [".", package', versionS, _] ->
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
                let p =
                        case parsePackageDescription $ T.unpack
                                $ T.decodeUtf8With T.lenientDecode bs of
                            ParseOk _ x -> Just x
                            _ -> Nothing
                 in Map.insert package' PackInfo
                        { piVersion = version
                        , piDesc = p
                        , piEpoch = Tar.entryTime entry
                        } m
            _ -> m

getDeps :: GenericPackageDescription -> [Dependency]
getDeps x = concat
          $ maybe id ((:) . condTreeConstraints) (condLibrary x)
          $ map (condTreeConstraints . snd) (condExecutables x)
