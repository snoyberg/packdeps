{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, RecordWildCards, OverloadedStrings, DeriveGeneric, FlexibleInstances, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Distribution.PackDeps.Types where

import ClassyPrelude.Conduit hiding (pi)
import GHC.Generics

import Distribution.Text (display)
import qualified Distribution.Types.PackageName as D
import qualified Distribution.Version as D
import Data.Vector ((!))

import Data.Binary (Binary (..), putWord8, getWord8, Put, Get)
import qualified Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.RWS hiding (get, put)
import qualified Data.Map as Map
import Control.DeepSeq

class GBinary f where
    gput :: f a -> Put
    gget :: Get (f a)
instance GBinary U1 where
    gput U1 = return ()
    gget = return U1
instance (GBinary a, GBinary b) => GBinary (a :*: b) where
    gput (a :*: b) = gput a >> gput b
    gget = (:*:) <$> gget <*> gget
instance (GBinary a, GBinary b) => GBinary (a :+: b) where
    gput (L1 x) = putWord8 0 >> gput x
    gput (R1 x) = putWord8 1 >> gput x
    gget = do
        b <- getWord8
        if b == 0 then L1 <$> gget else R1 <$> gget
instance Binary' a => GBinary (K1 i a) where
    gput (K1 x) = put' x
    gget = K1 <$> get'
instance GBinary a => GBinary (M1 i c a) where
    gput (M1 x) = gput x
    gget = M1 <$> gget

class Binary' a where
    put' :: a -> Put
    get' :: Get a

newtype NameId = NameId Word32
    deriving (Binary', Eq, Hashable, Enum, Bounded)
newtype VersionId = VersionId Word32
    deriving (Binary', Eq, Hashable, Enum, Bounded)
newtype LicenseId = LicenseId Word32
    deriving (Binary', Eq, Hashable, Enum, Bounded)

instance Binary' Int64 where
    put' = put
    get' = get
instance Binary' Word32 where
    put' = put
    get' = get
instance Binary' Int where
    put' = put
    get' = get
instance Binary' a => Binary' (Vector a) where
    put' v = put (length v) >> mapM_ put' v
    get' = do
        l <- get
        replicateM l get'
instance (Unbox a, Binary' a) => Binary' (UVector a) where
    put' v = put (length v) >> mapM_ put' v
    get' = do
        l <- get
        replicateM l get'
instance (Binary' a, Binary' b) => Binary' (a, b) where
    put' (a, b) = put' a >> put' b
    get' = (,) <$> get' <*> get'
instance Binary' a => Binary' (Maybe' a) where
    put' Nothing' = putWord8 0
    put' (Just' a) = putWord8 1 >> put' a
    get' = do
        x <- getWord8
        if x == 0 then return Nothing' else Just' <$> get'

data NewestIds = NewestIds !(Vector PackageName) !(Vector Version) !(Vector License) !(Vector (NameId, PackInfo NameId VersionId LicenseId))
    deriving Generic
instance Binary NewestIds where
    put = gput . from
    get = to <$> gget
instance Binary' (PackInfo NameId VersionId LicenseId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' (DescInfo NameId VersionId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' (VersionRange VersionId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' Text where
    put' = put . encodeUtf8
    get' = decodeUtf8 <$> get
instance (Eq k, Hashable k, Binary' k, Binary' v) => Binary' (HashMap k v) where
    put' = put' . asVector . pack . mapToList
    get' = mapFromList . unpack . asVector <$> get'
instance Binary' (PUVersionRange (VersionRange VersionId)) where
    put' = gput . from
    get' = to <$> gget
instance Binary' PackageUsage where
    put' = gput . from
    get' = to <$> gget

data NTI = NTI
    { ntiNextNameId :: NameId
    , ntiNextVersionId :: VersionId
    , ntiNextLicenseId :: LicenseId
    , ntiNameMap :: HashMap PackageName NameId
    , ntiVersionMap :: HashMap Version VersionId
    , ntiLicenseMap :: HashMap License LicenseId
    }

newestToIds :: Newest -> NewestIds
newestToIds (Newest newest) =
    NewestIds (pack $ namesDL []) (pack $ versionsDL []) (pack $ licensesDL []) (pack pairs)
  where
    (pairs, _, (namesDL, versionsDL, licensesDL)) = runRWS (mapM goPair $ mapToList newest) () (NTI minBound minBound minBound mempty mempty mempty)

    getName name = do
        nti <- RWS.get
        case lookup name $ ntiNameMap nti of
            Just nid -> return nid
            Nothing -> do
                let nid = ntiNextNameId nti
                RWS.put nti
                    { ntiNextNameId = succ nid
                    , ntiNameMap = insertMap name nid $ ntiNameMap nti
                    }
                tell ((name:), id, id)
                return nid

    getVersion version = do
        nti <- RWS.get
        case lookup version $ ntiVersionMap nti of
            Just vid -> return vid
            Nothing -> do
                let vid = ntiNextVersionId nti
                RWS.put nti
                    { ntiNextVersionId = succ vid
                    , ntiVersionMap = insertMap version vid $ ntiVersionMap nti
                    }
                tell (id, (version:), id)
                return vid

    getLicense license = do
        nti <- RWS.get
        case lookup license $ ntiLicenseMap nti of
            Just lid -> return lid
            Nothing -> do
                let lid = ntiNextLicenseId nti
                RWS.put nti
                    { ntiNextLicenseId = succ lid
                    , ntiLicenseMap = insertMap license lid $ ntiLicenseMap nti
                    }
                tell (id, id, (license:))
                return lid

    goPair (pn, pi) = do
        nid <- getName pn
        pi' <- goPI pi
        return (nid, pi')

    goPI pi = do
        v <- getVersion $ piVersion pi
        l <- getLicense $ piLicense pi
        di <- maybe' (return Nothing') (fmap Just' . goDI) $ piDesc pi
        return pi
            { piVersion = v
            , piDesc = di
            , piLicense = l
            }

    goDI di = do
        deps <- mapFromList <$> mapM goDep (mapToList $ diDeps di)
        return di { diDeps = deps }

    goDep (name, PUVersionRange pu vrange) = (,) <$> getName name <*> (PUVersionRange pu <$> goVR vrange)

    goVR AnyVersion = return AnyVersion
    goVR (ThisVersion v) = ThisVersion <$> getVersion v
    goVR (LaterVersion v) = LaterVersion <$> getVersion v
    goVR (EarlierVersion v) = EarlierVersion <$> getVersion v
    goVR (WildcardVersion v) = WildcardVersion <$> getVersion v
    goVR (UnionVersionRanges x y) = UnionVersionRanges <$> goVR x <*> goVR y
    goVR (IntersectVersionRanges x y) = IntersectVersionRanges <$> goVR x <*> goVR y
    goVR (VersionRangeParens x) = VersionRangeParens <$> goVR x

newestFromIds :: NewestIds -> Newest
newestFromIds (NewestIds nameV versionV licenseV pairs) =
    Newest $ mapFromList $ map goPair $ unpack pairs
  where
    getName (NameId nid) = nameV ! fromIntegral nid
    getVersion (VersionId vid) = versionV ! fromIntegral vid
    getLicense (LicenseId lid) = licenseV ! fromIntegral lid
    goPair (nid, p) = (getName nid, goPI p)

    goPI pi = pi
        { piVersion = getVersion $ piVersion pi
        , piDesc = fmap goDI $ piDesc pi
        , piLicense = getLicense $ piLicense pi
        }

    goDI di = di
        { diDeps = mapFromList $ map goPair' $ mapToList $ diDeps di
        }

    goPair' (nid, PUVersionRange pu vr) = (getName nid, PUVersionRange pu $ goVR vr)

    goVR AnyVersion = AnyVersion
    goVR (ThisVersion v) = ThisVersion $ getVersion v
    goVR (LaterVersion v) = LaterVersion $ getVersion v
    goVR (EarlierVersion v) = EarlierVersion $ getVersion v
    goVR (WildcardVersion v) = WildcardVersion $ getVersion v
    goVR (UnionVersionRanges x y) = UnionVersionRanges (goVR x) (goVR y)
    goVR (IntersectVersionRanges x y) = IntersectVersionRanges (goVR x) (goVR y)
    goVR (VersionRangeParens x) = VersionRangeParens (goVR x)

instance Binary Newest where
    put = put . newestToIds
    get = fmap newestFromIds get

-- | The newest version of every package.
newtype Newest = Newest { unNewest :: HashMap PackageName (PackInfo PackageName Version License) }

type Reverses = HashMap PackageName (Version, HashMap PackageName (VersionRange Version))

data PackInfo name version license = PackInfo
    { piVersion :: !version
    , piDesc :: !(Maybe' (DescInfo name version))
    , piEpoch :: !Int64
    , piLicense :: !license
    }
    deriving Generic

data Maybe' a = Just' !a | Nothing'
instance Functor Maybe' where
    fmap f (Just' x) = Just' (f x)
    fmap _ Nothing' = Nothing'

maybe' :: b -> (a -> b) -> Maybe' a -> b
maybe' b _ Nothing' = b
maybe' _ f (Just' a) = f a

-- | Information on a single package.
data DescInfo name version = DescInfo
    { diHaystack :: !Text
    , diDeps :: !(HashMap name (PUVersionRange (VersionRange version)))
    , diSynopsis :: !Text
    }
    deriving Generic

data PackageUsage = Runtime | TestBench
    deriving Generic
instance Monoid PackageUsage where
    mempty = TestBench
    Runtime `mappend` _ = Runtime
    TestBench `mappend` x = x

data PUVersionRange vr = PUVersionRange
    { puvrPU :: !PackageUsage
    , puvrVR :: !vr
    }
    deriving Generic
instance Monoid (PUVersionRange D.VersionRange) where
    mempty = PUVersionRange mempty D.AnyVersion
    PUVersionRange a x `mappend` PUVersionRange b y = PUVersionRange (mappend a b) (D.simplifyVersionRange $ D.IntersectVersionRanges x y)
instance Functor PUVersionRange where
    fmap f (PUVersionRange x y) = PUVersionRange x (f y)

-- FIXME add name caching function

data VersionRange version
  = AnyVersion
  | ThisVersion            !version -- = version
  | LaterVersion           !version -- > version  (NB. not >=)
  | EarlierVersion         !version -- < version
  | WildcardVersion        !version -- == ver.*   (same as >= ver && < ver+1)
  | UnionVersionRanges     !(VersionRange version) !(VersionRange version)
  | IntersectVersionRanges !(VersionRange version) !(VersionRange version)
  | VersionRangeParens     !(VersionRange version) -- just '(exp)' parentheses syntax
  deriving (Eq, Generic)

newtype PackageName = PackageName D.PackageName
    deriving (Read, Show, Eq, Ord, NFData)

instance Hashable PackageName where
  hashWithSalt i (PackageName x) = hashWithSalt i (D.unPackageName x)
instance Binary' PackageName where
  put' (PackageName pn) = put (D.unPackageName pn)
  get' = (PackageName . D.mkPackageName) <$> get

unPackageName :: PackageName -> Text
unPackageName (PackageName pn) = pack $ D.unPackageName pn

mkPackageName :: Text -> PackageName
mkPackageName = PackageName . D.mkPackageName . unpack

newtype Version = Version D.Version
    deriving (Eq, Ord, Generic)

instance Binary' Version where
    put' (Version v) = put $ D.versionNumbers v
    get' = (Version . D.mkVersion) <$> get

instance Hashable Version where
    hashWithSalt i (Version x) = hashWithSalt i $ D.versionNumbers x

instance Show Version where
    show (Version x) = display x

m'ToM :: Maybe' a -> Maybe a
m'ToM Nothing' = Nothing
m'ToM (Just' a) = Just a

instance Show (VersionRange Version) where
    show =
        display . unVR
      where
        unVR AnyVersion = D.AnyVersion
        unVR (ThisVersion v) = D.ThisVersion (unV v)
        unVR (LaterVersion v) = D.LaterVersion (unV v)
        unVR (EarlierVersion v) = D.EarlierVersion (unV v)
        unVR (WildcardVersion v) = D.WildcardVersion (unV v)
        unVR (UnionVersionRanges x y) = D.UnionVersionRanges (unVR x) (unVR y)
        unVR (IntersectVersionRanges x y) = D.IntersectVersionRanges (unVR x) (unVR y)
        unVR (VersionRangeParens x) = D.VersionRangeParens (unVR x)

        unV (Version x) = x

newtype License = License { unLicense :: Text }
    deriving (Show, Eq, Ord, Binary', Hashable, NFData)
newtype Licenses = Licenses { unLicenses :: (Map License (Set PackageName)) }
    deriving (NFData)

instance Monoid Licenses where
    mempty = Licenses mempty
    Licenses x `mappend` Licenses y = Licenses $ Map.unionWith mappend x y

type LicenseMap = Map PackageName Licenses
