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
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Distribution.PackDeps (Newest, Reverses, loadNewestFrom, getReverses, DescInfo, getLicenseMap)
import Distribution.PackDeps.Types
import Control.Monad (forever)
import Control.DeepSeq (NFData (rnf), deepseq, ($!!))
import Data.Version (Version (..))
import Distribution.Version (VersionRange)
import Network.HTTP.Conduit
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkFile)
import System.IO (hPutStrLn, stderr, hFlush)
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary

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

loadData :: ((Newest, Reverses, (LicenseMap, LicenseMap)) -> IO ())
         -> IO ()
loadData update' = do
    let log s = hPutStrLn stderr s >> hFlush stderr
    log "Entered loadData"
    req' <- parseUrl "http://hackage.haskell.org/packages/archive/00-index.tar.gz"
    let req = req' { decompress = alwaysDecompress, responseTimeout = Just 30000000 }
    forever $ do
        log "In forever"
        res <- try $ do
            withManager $ \m -> do
                res <- http req m
                liftIO $ log "Received response headers"
                responseBody res $$+- sinkFile "tmp"
            log "Finished writing"
            !newest <- fmap (newestFromIds . newestToIds) $ loadNewestFrom "tmp"
            log "Finished parsing"
            !reverses <- return $! getReverses newest
            log "Finished making reverses"
            !licenses1 <- return $!! getLicenseMap False newest
            !licenses2 <- return $!! getLicenseMap True newest
            log "Finished making license map"
            update' (newest, reverses, (licenses1, licenses2))
            _ <- forkIO $ L.writeFile cacheFile $ Data.Binary.encode newest
            log "Updated"
            threadDelay $ 1000 * 1000 * 60 * 60
        case res of
            Left (e :: SomeException) -> do
                log $ "Received exception: " ++ show e
                threadDelay $ 1000 * 1000 * 30
            Right () -> return ()

cacheFile :: FilePath
cacheFile = "/tmp/packdeps-cache.bin"

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    s <- staticSite
    edata <- try $ do
        lbs <- L.readFile cacheFile
        !newest <- return $! Data.Binary.decode lbs
        return $! newest
    mdata <-
        case edata of
            Left (e :: SomeException) -> do
                hPutStrLn stderr $ "Failed initial load: " ++ show e
                return Nothing
            Right x -> return $ Just (x, getReverses x, (getLicenseMap False x, getLicenseMap True x))
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
