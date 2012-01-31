import Distribution.PackDeps
import Control.Monad (forM_)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Distribution.Text (display)
import Distribution.Package (PackageName (PackageName))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usageExit
        ["help"] -> usageExit
        _ -> run args

run :: [String] -> IO ()
run args = do
    newest <- loadNewest
    mapM_ (go newest) args
  where
    go newest fp = do
        mdi <- loadPackage fp
        di <-
            case mdi of
                Just di -> return di
                Nothing -> error $ "Could not parse cabal file: " ++ fp
        allGood <- case checkDeps newest di of
            (pn, v, AllNewest) -> do
                putStrLn $ concat
                    [ unPackageName pn
                    , "-"
                    , display v
                    , ": Can use newest versions of all dependencies"
                    ]
                return True
            (pn, v, WontAccept p _) -> do
                putStrLn $ concat
                    [ unPackageName pn
                    , "-"
                    , display v
                    , ": Cannot accept the following packages"
                    ]
                forM_ p $ \(x, y) -> putStrLn $ x ++ " " ++ y
                return False
        putStrLn ""
        if allGood
            then exitSuccess
            else exitFailure

unPackageName :: PackageName -> String
unPackageName (PackageName n) = n


usageExit :: IO a
usageExit = do
    pname <- getProgName
    putStrLn $ "\n"
        ++ "Usage: " ++ pname ++ " pkgname.cabal\n\n"
        ++ "Check the given cabal file's dependency list to make sure that it does not exclude\n"
        ++ "the newest package available. Its probably worth running the 'cabal update' command\n"
        ++ "immediately before running this program.\n"
    exitSuccess
