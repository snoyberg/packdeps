import Distribution.PackDeps
import Control.Monad (forM_, foldM, when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import Distribution.Text (display)
import Distribution.Package (PackageName (PackageName))
import Distribution.Version (Version)
import Control.Monad (liftM)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usageExit
        ["help"] -> usageExit
        _ | "-h" `elem` args || "--help" `elem` args -> usageExit
        _ -> do
            isGood <- run ("--recursive" `elem` args) ("--quiet" `elem` args) (filter (\arg -> arg /= "--recursive" && arg /= "--quiet") args)
            if isGood then exitSuccess else exitFailure

type CheckDeps = Newest -> DescInfo -> (PackageName, Version, CheckDepsRes)

checkDepsCli :: Bool -> CheckDeps -> Newest -> DescInfo -> IO Bool
checkDepsCli quiet cd newest di =
    case cd newest di of
        (pn, v, AllNewest)
          | quiet -> return True
          | otherwise -> do
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

run :: Bool        -- ^ Check transitive dependencies
    -> Bool        -- ^ Quiet -- only report packages that are not up to date
    -> [FilePath]  -- ^ .cabal filenames
    -> IO Bool
run deep quiet args = do
    newest <- loadNewest
    foldM (go newest) True args
  where
    go newest wasAllGood fp = do
        mdi <- loadPackage fp
        di <- case mdi of
             Just di -> return di
             Nothing -> error $ "Could not parse cabal file: " ++ fp
        allGood <- checkDepsCli quiet checkDeps newest di
        depsGood <- if deep
                       then do putStrLn $ "\nTransitive dependencies:"
                               allM (checkDepsCli quiet checkLibDeps newest) (deepLibDeps newest [di])
                       else return True
        when (not (allGood && depsGood)) $ putStrLn ""
        return $ wasAllGood && allGood && depsGood

unPackageName :: PackageName -> String
unPackageName (PackageName n) = n


usageExit :: IO a
usageExit = do
    pname <- getProgName
    putStrLn $ "\n"
        ++ "Usage: " ++ pname ++ " [--recursive] [--quiet] pkgname.cabal pkgname2.cabal...\n\n"
        ++ "Check the given cabal file's dependency list to make sure that it does not exclude\n"
        ++ "the newest package available. Its probably worth running the 'cabal update' command\n"
        ++ "immediately before running this program.\n\n"
        ++ "  --quiet\n"
        ++ "      Suppress output for .cabal files which can accept the newest packages available.\n"
    exitSuccess

-- | Non short-circuiting monadic version of 'all'
-- Duh, pre-AMP code.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = and `liftM` mapM f xs
