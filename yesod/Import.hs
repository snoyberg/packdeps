module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Settings.Development
    , module Data.Monoid
    , module Control.Applicative
    , Text
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
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

import Data.Time (UTCTime)
import Distribution.PackDeps
    ( Newest, Reverses, DescInfo, CheckDepsRes (AllNewest, WontAccept)
    , filterPackages, deepDeps, checkDeps
    )
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import Distribution.Version (Version)
import Distribution.Package (PackageName (PackageName))
import Data.IORef (readIORef)

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

isDeep :: Handler Bool
isDeep = fmap (== Just "on") $ runInputGet $ iopt textField "deep"

getData :: Handler (Newest, Reverses)
getData = do
    mdata <- (appData <$> getYesod) >>= liftIO . readIORef
    maybe (error "Still loading data, please wait") return mdata

getDeps :: Bool
        -> String
        -> Handler ([DescInfo], [((String, Version), ([(String, String)], UTCTime))])
getDeps deep needle = do
    (newest, _) <- getData
    let descs' = filterPackages needle newest
        descs = if deep then deepDeps newest descs' else descs'
        go (_, _, AllNewest) = Nothing
        go (PackageName x, v, WontAccept y z) = Just ((x, v), (y, z))
        deps = reverse $ sortBy (comparing $ snd . snd)
             $ mapMaybe (go . checkDeps newest) descs
    return (descs, deps)
