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
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback, logCallbackDev)
import qualified Data.IORef as I

import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString as S
import Distribution.PackDeps (Newest, Reverses, loadNewest, getReverses)
import Control.Monad (forever)
import System.Process (rawSystem)

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
loadData log load = forever $ do
    log "cabal update"
    _ <- rawSystem "cabal" ["update"]
    log "Loading data"
    newest <- loadNewest
    log "Loaded newest"
    let reverses = getReverses newest
    log "Get reverses"
    load (newest, reverses)
    log "Loaded, sleeping"
    threadDelay $ 1000 * 1000 * 60 * 60

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
