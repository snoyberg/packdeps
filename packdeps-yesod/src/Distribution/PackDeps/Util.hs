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
convertVersionRange = D.cataVersionRange goR
  where
    goR (D.ThisVersionF x) = ThisVersion $ convertVersion x
    goR (D.LaterVersionF x) = LaterVersion $ convertVersion x
    goR (D.OrLaterVersionF x) = OrLaterVersion $ convertVersion x
    goR (D.EarlierVersionF x) = EarlierVersion $ convertVersion x
    goR (D.OrEarlierVersionF x) = OrEarlierVersion $ convertVersion x
    goR (D.MajorBoundVersionF x) = MajorBoundVersion $ convertVersion x
    goR (D.UnionVersionRangesF x y) = UnionVersionRanges x y
    goR (D.IntersectVersionRangesF x y) = IntersectVersionRanges x y

convertVersion :: D.Version -> Version
convertVersion = Version
