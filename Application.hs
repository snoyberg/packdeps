{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
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
import qualified Data.IORef as I

import Control.Exception (SomeException, try)
import Control.Concurrent (forkIO, threadDelay)
import Distribution.PackDeps (loadNewestFrom, getReverses, getLicenseMap)
import Distribution.PackDeps.Types
import Control.Monad (forever)
import Control.DeepSeq (($!!))
import Network.HTTP.Conduit
import Network.HTTP.Client.TLS (getGlobalManager)
import Data.Conduit (($$+-), (=$))
import Data.Conduit.Binary (sinkFile)
import Data.Conduit.Zlib (ungzip)
import System.IO (hPutStrLn, stderr, hFlush)
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary
import Data.Time (getCurrentTime, UTCTime (..), Day (..))
import ClassyPrelude.Conduit (runResourceT)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.License

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    _ <- forkIO $ loadData $ I.writeIORef (appData foundation) . Just
    return app

loadData :: ((Newest, Reverses, (LicenseMap, LicenseMap), UTCTime) -> IO ())
         -> IO ()
loadData update' = do
    let log s = do
            now <- getCurrentTime
            hPutStrLn stderr $ concat [show now, ": ", s]
            hFlush stderr
    log "Entered loadData"
    req' <- parseUrlThrow "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    let req = req' { responseTimeout = Just 30000000 }
    forever $ do
        log "In forever"
        res <- try $ do
            m <- getGlobalManager
            runResourceT $ do
                res <- http req m
                liftIO $ log "Received response headers"
                responseBody res $$+- ungzip =$ sinkFile "tmp"
            log "Finished writing"
            !newest <- fmap (newestFromIds . newestToIds) $ loadNewestFrom "tmp"
            log "Finished parsing"
            !reverses <- return $! getReverses newest
            log "Finished making reverses"
            !licenses1 <- return $!! getLicenseMap False newest
            !licenses2 <- return $!! getLicenseMap True newest
            log "Finished making license map"
            now <- getCurrentTime
            update' (newest, reverses, (licenses1, licenses2), now)
            _ <- forkIO $ L.writeFile cacheFile $ Data.Binary.encode (newest, now)
            log "Updated"
            threadDelay $ 1000 * 1000 * 60 * 60
        case res of
            Left (e :: SomeException) -> do
                log $ "Received exception: " ++ show e
                threadDelay $ 1000 * 1000 * 30
            Right () -> return ()

cacheFile :: FilePath
cacheFile = "/tmp/packdeps-cache.bin"

instance Data.Binary.Binary UTCTime where
    put (UTCTime (ModifiedJulianDay day) time) = do
        Data.Binary.put day
        Data.Binary.put (round time :: Integer)
    get = do
        day <- Data.Binary.get
        time <- Data.Binary.get
        return $ UTCTime (ModifiedJulianDay day) (fromInteger time)

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    s <- staticSite
    edata <- try $ do
        lbs <- L.readFile cacheFile
        (newest, timestamp) <- return $! Data.Binary.decode lbs
        newest `seq` timestamp `seq` return (newest, timestamp)
    mdata <-
        case edata of
            Left (e :: SomeException) -> do
                hPutStrLn stderr $ "Failed initial load: " ++ show e
                return Nothing
            Right (x, timestamp) -> return $ Just
                ( x
                , getReverses x
                , (getLicenseMap False x, getLicenseMap True x)
                , timestamp
                )
    idata <- I.newIORef mdata
    return $ App conf s idata

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
