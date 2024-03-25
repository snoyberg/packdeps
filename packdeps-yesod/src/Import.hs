module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
    , isDeep
    , getDeps
    , getData
    , pack
    ) where

import Prelude hiding (writeFile, readFile, head, tail, init, last)
import Yesod   hiding (Route(..))
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text, pack)
import Settings.StaticFiles
import Settings.Development
import Data.HashMap.Strict (HashMap)

import Data.Time (UTCTime)
import Distribution.PackDeps
    ( Newest, Reverses, DescInfo, CheckDepsRes (AllNewest, WontAccept)
    , filterPackages, deepDeps, checkDeps, Outdated
    )
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import Distribution.PackDeps.Types (PackageName (PackageName), Version)
import Distribution.Types.PackageName (unPackageName)
import Data.IORef (readIORef)

isDeep :: Handler Bool
isDeep = fmap (== Just "on") $ runInputGet $ iopt textField "deep"

getData :: Handler (Newest, Reverses)
getData = do
    mdata <- (appData <$> getYesod) >>= liftIO . readIORef
    maybe (error "Still loading data, please wait") (\(a, b, _, _) -> return (a, b)) mdata

getDeps :: Bool
        -> Text
        -> Handler
            ( [(PackageName, Version, DescInfo PackageName Version)]
            , [((Text, Version), (HashMap PackageName Outdated, UTCTime))]
            )
getDeps deep needle = do
    (newest, _) <- getData
    let descs' = filterPackages needle newest
        descs = if deep then deepDeps newest descs' else descs'
        go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((pack $ unPackageName x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    return (descs, deps)
