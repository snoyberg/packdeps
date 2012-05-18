{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import hiding (log)
import Settings
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Yesod.Logger (Logger, logBS, toProduction, flushLogger)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import qualified Data.IORef as I

import Control.Exception (SomeException, try)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Distribution.PackDeps (Newest, Reverses, loadNewestFrom, getReverses, PackInfo (..), DescInfo, diName)
import Control.Monad (forever)
import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Version (Version (..))
import Distribution.Version (VersionRange)
import Network.HTTP.Conduit
import Data.Conduit (($$))
import Data.Conduit.Binary (sinkFile)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
makeApplication conf logger = do
    _ <- forkIO $ forever $ do
        threadDelay $ 1000 * 1000 * 10
        flushLogger logger

    foundation <- makeFoundation conf setLogger
    app <- toWaiAppPlain foundation
    _ <- forkIO $ loadData log $ I.writeIORef (appData foundation) . Just
    return $ logWare app
  where
    setLogger = if development then logger else toProduction logger
    log       = logBS setLogger
    logWare   = if development then logCallbackDev log
                               else logCallback    log

loadData :: (S.ByteString -> IO ())
         -> ((Newest, Reverses) -> IO ())
         -> IO ()
loadData log update' = do
    req' <- parseUrl "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    let req = req' { decompress = alwaysDecompress }
    forever $ do
        res <- try $ do
            log "Downloading newest package list"
            withManager $ \m -> do
                res <- http req m
                responseBody res $$ sinkFile "tmp"
            log "Loading data"
            newest <- loadNewestFrom "tmp"
            newest `deepseq` log "Loaded newest"
            let reverses = getReverses newest
            reverses `deepseq` log "Loaded reverses"
            update' (newest, reverses)
            log "Updated, sleeping"
            threadDelay $ 1000 * 1000 * 60 * 60
        case res of
            Left e -> do
                log $ S8.pack $ show (e :: SomeException)
                threadDelay $ 1000 * 1000 * 30
            Right () -> return ()

makeFoundation :: AppConfig DefaultEnv Extra -> Logger -> IO App
makeFoundation conf setLogger = do
    s <- staticSite
    idata <- I.newIORef Nothing
    return $ App conf setLogger s idata

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

-- orphans
instance NFData PackInfo where
    rnf (PackInfo a b c) = a `deepseq` b `deepseq` c `deepseq` ()
instance NFData DescInfo where
    rnf d = diName d `deepseq` ()
instance NFData Version where
    rnf (Version a b) = a `deepseq` b `deepseq` ()
instance NFData VersionRange
