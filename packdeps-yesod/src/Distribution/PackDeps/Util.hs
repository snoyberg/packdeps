{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Distribution.PackDeps.Util where

import ClassyPrelude.Conduit
import Distribution.PackDeps.Types
import qualified Distribution.Version as D
import qualified Prelude

withinRange :: Version -> VersionRange Version -> Bool
withinRange v = foldVersionRange
                   True
                   (\v'  -> versionBranch v == versionBranch v')
                   (\v'  -> versionBranch v >  versionBranch v')
                   (\v'  -> versionBranch v <  versionBranch v')
                   (||)
                   (&&)
  where
    versionBranch (Version x) = D.versionNumbers x

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
    fold' (MajorBoundVersion v)          = fold' (majorBound v)
    fold' (OrLaterVersion v)             = fold' (orLaterVersion v)
    fold' (OrEarlierVersion v)           = fold' (orEarlierVersion v)

    wildcard v = IntersectVersionRanges
                   (orLaterVersion v)
                   (EarlierVersion (wildcardUpperBound v))

    majorBound v = IntersectVersionRanges
                    (orLaterVersion v)
                    (EarlierVersion (majorUpperBound v))

wildcardUpperBound :: Version -> Version
wildcardUpperBound (Version (D.versionNumbers -> lowerBound)) = (Version $ D.mkVersion upperBound)
  where
    upperBound = Prelude.init lowerBound ++ [Prelude.last lowerBound + 1]

majorUpperBound :: Version -> Version
majorUpperBound (Version (D.versionNumbers -> v)) =
  Version $ D.mkVersion $
    case v of
      [] -> error "majorUpperbound: impossible empty input"
      [x] -> [x, 1]
      x:y:_ -> [x, y + 1]

orLaterVersion :: Version -> VersionRange Version
orLaterVersion   v = UnionVersionRanges (ThisVersion v) (LaterVersion v)

orEarlierVersion :: Version -> VersionRange Version
orEarlierVersion   v = UnionVersionRanges (ThisVersion v) (EarlierVersion v)

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
    goR (D.MajorBoundVersion x) = MajorBoundVersion $ convertVersion x
    goR (D.OrLaterVersion x) = OrLaterVersion $ convertVersion x
    goR (D.OrEarlierVersion x) = OrEarlierVersion $ convertVersion x

convertVersion :: D.Version -> Version
convertVersion = Version
