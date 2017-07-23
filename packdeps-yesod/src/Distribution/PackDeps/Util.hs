{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Distribution.PackDeps.Util where

import ClassyPrelude.Conduit
import Distribution.PackDeps.Types
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Distribution.Version as D

withinRange :: Version -> VersionRange Version -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> versionBranch v == versionBranch v')
                   (\v'  -> versionBranch v >  versionBranch v')
                   (\v'  -> versionBranch v <  versionBranch v')
                   (||)
                   (&&)

foldVersionRange :: a                         -- ^ @\"-any\"@ version
                 -> (Version -> a)            -- ^ @\"== v\"@
                 -> (Version -> a)            -- ^ @\"> v\"@
                 -> (Version -> a)            -- ^ @\"< v\"@
                 -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                 -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                 -> VersionRange Version -> a
foldVersionRange anyv this later earlier union' intersect' = fold'
  where
    fold' AnyVersion                     = anyv
    fold' (ThisVersion v)                = this v
    fold' (LaterVersion v)               = later v
    fold' (EarlierVersion v)             = earlier v
    fold' (WildcardVersion v)            = fold' (wildcard v)
    fold' (UnionVersionRanges v1 v2)     = union' (fold' v1) (fold' v2)
    fold' (IntersectVersionRanges v1 v2) = intersect' (fold' v1) (fold' v2)
    fold' (VersionRangeParens v)         = fold' v

    wildcard v = IntersectVersionRanges
                   (orLaterVersion v)
                   (EarlierVersion (wildcardUpperBound v))


foldVersionRange' :: a                         -- ^ @\"-any\"@ version
                  -> (Version -> a)            -- ^ @\"== v\"@
                  -> (Version -> a)            -- ^ @\"> v\"@
                  -> (Version -> a)            -- ^ @\"< v\"@
                  -> (Version -> a)            -- ^ @\">= v\"@
                  -> (Version -> a)            -- ^ @\"<= v\"@
                  -> (Version -> Version -> a) -- ^ @\"== v.*\"@ wildcard. The
                                               -- function is passed the
                                               -- inclusive lower bound and the
                                               -- exclusive upper bounds of the
                                               -- range defined by the wildcard.
                  -> (a -> a -> a)             -- ^ @\"_ || _\"@ union
                  -> (a -> a -> a)             -- ^ @\"_ && _\"@ intersection
                  -> (a -> a)                  -- ^ @\"(_)\"@ parentheses
                  -> VersionRange Version -> a
foldVersionRange' anyv this later earlier orLater orEarlier
                  wildcard union' intersect' parens = fold'
  where
    fold' AnyVersion                     = anyv
    fold' (ThisVersion v)                = this v
    fold' (LaterVersion v)               = later v
    fold' (EarlierVersion v)             = earlier v

    fold' (UnionVersionRanges (ThisVersion    v)
                             (LaterVersion   v')) | v==v' = orLater v
    fold' (UnionVersionRanges (LaterVersion   v)
                             (ThisVersion    v')) | v==v' = orLater v
    fold' (UnionVersionRanges (ThisVersion    v)
                             (EarlierVersion v')) | v==v' = orEarlier v
    fold' (UnionVersionRanges (EarlierVersion v)
                             (ThisVersion    v')) | v==v' = orEarlier v

    fold' (WildcardVersion v)            = wildcard v (wildcardUpperBound v)
    fold' (UnionVersionRanges v1 v2)     = union' (fold' v1) (fold' v2)
    fold' (IntersectVersionRanges v1 v2) = intersect' (fold' v1) (fold' v2)
    fold' (VersionRangeParens v)         = parens (fold' v)

wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version lowerBound ts) = (Version upperBound ts)
  where
    upperBound = UVector.init lowerBound `UVector.snoc` (UVector.last lowerBound + 1)

orLaterVersion :: Version -> VersionRange Version
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

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
convertVersion (D.Version x y) = Version (UVector.fromList x) (Vector.fromList $ map pack y)
