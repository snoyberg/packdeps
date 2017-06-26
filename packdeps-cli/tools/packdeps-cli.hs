import Control.Applicative (some)
import Control.Monad (forM_, foldM, when)
import Control.Monad (liftM)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.PackDeps
import Distribution.Package (PackageIdentifier (PackageIdentifier), PackageName (PackageName))
import Distribution.Text (display, parse)
import Distribution.Version (Version)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)

import qualified Data.Map as Map
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
        <*> O.switch (O.long "ghc-pkg" <> O.help "Use ghc-pkg list to additionally populate the newest packages database.")
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

-- TODO
updateWithGhcPkg :: Newest -> IO Newest
updateWithGhcPkg newest = do
    output <- readProcess "ghc-pkg" ["list", "--simple-output"] ""
    let pkgs = mapMaybe parseSimplePackInfo (words output)
    return $ foldl' apply newest pkgs
  where
    apply :: Newest -> (String, PackInfo) -> Newest
    apply m (k, v) = Map.insert k v m

    parseSimplePackInfo :: String -> Maybe (String, PackInfo)
    parseSimplePackInfo str =
        case filter ((== "") . snd) $ readP_to_S parse str of
            ((PackageIdentifier (PackageName name) ver, _) : _) ->
                Just (name, PackInfo ver Nothing 0)
            _ -> Nothing

run :: Bool        -- ^ Check transitive dependencies
    -> Bool        -- ^ Quiet -- only report packages that are not up to date
    -> Bool        -- ^ ghc-pkg -- use ghc-pkg list to see whether there are even newer packages.
    -> [FilePath]  -- ^ .cabal filenames
    -> IO Bool
run deep quiet ghcpkg args = do
    newest' <- loadNewest
    newest <- if ghcpkg then updateWithGhcPkg newest' else return newest'
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
