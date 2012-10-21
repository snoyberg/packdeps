{-# LANGUAGE NoImplicitPrelude, GeneralizedNewtypeDeriving, RecordWildCards, OverloadedStrings, DeriveGeneric, FlexibleInstances, TypeOperators #-}
module Distribution.PackDeps.Types where

import ClassyPrelude.Conduit
import GHC.Generics
import Prelude (zip)

import Prelude (show)
import Distribution.Text (display)
import qualified Distribution.Version as D
import qualified Data.HashMap.Strict as H
import Data.Vector (replicateM, (!))

import Data.Binary
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.RWS hiding (get, put)
import Data.Hashable (hashWithSalt)

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
instance (Binary' a, Binary' b) => Binary' (a, b) where
    put' (a, b) = put' a >> put' b
    get' = (,) <$> get' <*> get'
instance Binary' a => Binary' (Maybe' a) where
    put' Nothing' = putWord8 0
    put' (Just' a) = putWord8 1 >> put' a
    get' = do
        x <- getWord8
        if x == 0 then return Nothing' else Just' <$> get'

data NewestIds = NewestIds !(Vector PackageName) !(Vector Version) !(Vector (NameId, PackInfo NameId VersionId))
    deriving Generic
instance Binary NewestIds where
    put = gput . from
    get = to <$> gget
instance Binary' (PackInfo NameId VersionId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' (DescInfo NameId VersionId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' (VersionRange VersionId) where
    put' = gput . from
    get' = to <$> gget
instance Binary' Version where
    put' = gput . from
    get' = to <$> gget
instance Binary' Text where
    put' = put . encodeUtf8
    get' = decodeUtf8 <$> get
instance (Eq k, Hashable k, Binary' k, Binary' v) => Binary' (HashMap k v) where
    put' = put' . asVector . pack . unpack
    get' = pack . unpack . asVector <$> get'

data NTI = NTI
    { ntiNextNameId :: NameId
    , ntiNextVersionId :: VersionId
    , ntiNameMap :: HashMap PackageName NameId
    , ntiVersionMap :: HashMap Version VersionId
    }

newestToIds :: Newest -> NewestIds
newestToIds (Newest newest) =
    NewestIds (pack $ namesDL []) (pack $ versionsDL []) (pack pairs)
  where
    (pairs, _, (namesDL, versionsDL)) = runRWS (mapM goPair $ unpack newest) () (NTI minBound minBound empty empty)

    getName name = do
        nti <- RWS.get
        case lookup name $ ntiNameMap nti of
            Just nid -> return nid
            Nothing -> do
                let nid = ntiNextNameId nti
                RWS.put nti
                    { ntiNextNameId = succ nid
                    , ntiNameMap = insert name nid $ ntiNameMap nti
                    }
                tell ((name:), id)
                return nid

    getVersion version = do
        nti <- RWS.get
        case lookup version $ ntiVersionMap nti of
            Just vid -> return vid
            Nothing -> do
                let vid = ntiNextVersionId nti
                RWS.put nti
                    { ntiNextVersionId = succ vid
                    , ntiVersionMap = insert version vid $ ntiVersionMap nti
                    }
                tell (id, (version:))
                return vid

    goPair (pn, pi) = do
        nid <- getName pn
        pi' <- goPI pi
        return (nid, pi')

    goPI pi = do
        v <- getVersion $ piVersion pi
        di <- maybe' (return Nothing') (fmap Just' . goDI) $ piDesc pi
        return pi
            { piVersion = v
            , piDesc = di
            }

    goDI di = do
        deps <- pack <$> mapM goDep (unpack $ diDeps di)
        return di { diDeps = deps }

    goDep (name, vrange) = (,) <$> getName name <*> goVR vrange

    goVR AnyVersion = return AnyVersion
    goVR (ThisVersion v) = ThisVersion <$> getVersion v
    goVR (LaterVersion v) = LaterVersion <$> getVersion v
    goVR (EarlierVersion v) = EarlierVersion <$> getVersion v
    goVR (WildcardVersion v) = WildcardVersion <$> getVersion v
    goVR (UnionVersionRanges x y) = UnionVersionRanges <$> goVR x <*> goVR y
    goVR (IntersectVersionRanges x y) = IntersectVersionRanges <$> goVR x <*> goVR y
    goVR (VersionRangeParens x) = VersionRangeParens <$> goVR x

newestFromIds :: NewestIds -> Newest
newestFromIds (NewestIds nameV versionV pairs) =
    Newest $ pack $ map goPair $ unpack pairs
  where
    getName (NameId nid) = nameV ! fromIntegral nid
    getVersion (VersionId vid) = versionV ! fromIntegral vid
    goPair (nid, p) = (getName nid, goPI p)

    goPI pi = pi
        { piVersion = getVersion $ piVersion pi
        , piDesc = fmap goDI $ piDesc pi
        }

    goDI di = di
        { diDeps = pack $ map goPair' $ unpack $ diDeps di
        }

    goPair' (nid, vr) = (getName nid, goVR vr)

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
newtype Newest = Newest { unNewest :: HashMap PackageName (PackInfo PackageName Version) }

type Reverses = HashMap PackageName (Version, HashMap PackageName (VersionRange Version))

data PackInfo name version = PackInfo
    { piVersion :: !version
    , piDesc :: !(Maybe' (DescInfo name version))
    , piEpoch :: !Int64
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
    , diDeps :: !(HashMap name (VersionRange version))
    , diSynopsis :: !Text
    }
    deriving Generic

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

newtype PackageName = PackageName { unPackageName :: Text }
    deriving (Read, Show, Eq, Ord, Hashable, Binary')

data Version = Version
    { versionBranch :: !(Vector Int)
    , versionTags   :: !(Vector Text)
    }
    deriving (Eq, Ord, Generic)

instance Hashable Version where
    hashWithSalt i (Version x y) = hashWithSalt i (unpack x, unpack y)

instance Show Version where
    show (Version x y) = display $ D.Version (unpack x) (map unpack $ unpack y)

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

        unV (Version x y) = D.Version (unpack x) (map unpack $ unpack y)
