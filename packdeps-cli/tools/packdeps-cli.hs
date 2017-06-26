import Control.Applicative (some)
import Distribution.PackDeps
import Control.Monad (forM_, foldM, when)
import System.Exit (exitFailure, exitSuccess)
import Distribution.Text (display)
import Distribution.Package (PackageName (PackageName))
import Distribution.Version (Version)
import Control.Monad (liftM)
import Data.Semigroup ((<>))

import qualified Options.Applicative as O

main :: IO ()
main = do
    run' <- O.execParser $ O.info (O.helper <*> opts) $ mconcat
        [ O.fullDesc
        , O.progDesc $ unwords
            [ "Check the given cabal file's dependency list to make sure that it does not exclude"
            , "the newest package available. It's probably worth running the 'cabal update' command"
            , "immediately before running this program."
            ]
        ]
    isGood <- run'
    if isGood then exitSuccess else exitFailure
  where
    opts = run
        <$> O.switch (O.long "recursive" <> O.help "Check transitive dependencies as well")
        <*> O.switch (O.long "quiet" <> O.help "Suppress output for .cabal files which can accept the newest packages available.")
        <*> some (O.strArgument (O.metavar "pkgname.cabal"))

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

-- | Non short-circuiting monadic version of 'all'
-- Duh, pre-AMP code.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = and `liftM` mapM f xs
