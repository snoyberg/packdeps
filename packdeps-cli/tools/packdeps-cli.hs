{-# LANGUAGE ViewPatterns #-}
import Control.Applicative (many, some)
import Control.Monad (forM_, foldM, when)
import Control.Monad (liftM)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Semigroup ((<>))
import Distribution.Compat.ReadP (readP_to_S)
import Distribution.PackDeps
import Distribution.Package (PackageIdentifier (PackageIdentifier), PackageName, unPackageName)
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
        <$> O.switch (O.short 'r' <> O.long "recursive" <> O.help "Check transitive dependencies as well.")
        <*> O.switch (O.short 'q' <> O.long "quiet" <> O.help "Suppress output for .cabal files which can accept the newest packages available.")
        <*> O.switch (O.short 'g' <> O.long "ghc-pkg" <> O.help "Use ghc-pkg list to additionally populate the newest packages database.")
        <*> O.switch (O.short 'p' <> O.long "preferred" <> O.help "Consider only preferred versions")
        <*> many (O.strOption (O.short 'e' <> O.long "exclude" <> O.metavar "pkgname" <> O.help "Exclude the package from the output."))
        <*> some (O.strArgument (O.metavar "pkgname.cabal"))

type CheckDeps = Newest -> DescInfo -> (PackageName, Version, CheckDepsRes)

checkDepsCli :: Bool -> [String] -> CheckDeps -> Newest -> DescInfo -> IO Bool
checkDepsCli quiet excludes cd newest di =
    case overTrd filterExcludes $ cd newest di of
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
            forM_ p $ \(x, y) -> putStrLn $ display x ++ " " ++ display y
            return False
  where
    overTrd :: (a -> b) -> (x, y, a) -> (x, y, b)
    overTrd f (x, y, a) = (x, y, f a)

    filterExcludes :: CheckDepsRes -> CheckDepsRes
    filterExcludes AllNewest = AllNewest
    filterExcludes (WontAccept p t) =
        case filter (\(n, _) -> display n `notElem` excludes) p of
            [] -> AllNewest
            p' -> WontAccept p' t

-- TODO
updateWithGhcPkg :: Newest -> IO Newest
updateWithGhcPkg newest = do
    output <- readProcess "ghc-pkg" ["list", "--simple-output"] ""
    let pkgs = mapMaybe parseSimplePackInfo (words output)
    return $ foldl' apply newest pkgs
  where
    apply :: Newest -> (PackageName, PackInfo) -> Newest
    apply m (k, v) = Map.insert k v m

    parseSimplePackInfo :: String -> Maybe (PackageName, PackInfo)
    parseSimplePackInfo str =
        case filter ((== "") . snd) $ readP_to_S parse str of
            ((PackageIdentifier name ver, _) : _) ->
                Just (name, PackInfo ver Nothing 0)
            _ -> Nothing

run :: Bool        -- ^ Check transitive dependencies
    -> Bool        -- ^ Quiet -- only report packages that are not up to date
    -> Bool        -- ^ ghc-pkg -- use ghc-pkg list to see whether there are even newer packages.
    -> Bool        -- ^ preferred -- consider only preferred versions
    -> [String]    -- ^ packages to exclude
    -> [FilePath]  -- ^ .cabal filenames
    -> IO Bool
run deep quiet ghcpkg preferred excludes args = do
    newest' <- loadNewest preferred
    newest <- if ghcpkg then updateWithGhcPkg newest' else return newest'
    foldM (go newest) True args
  where
    go newest wasAllGood fp = do
        mdi <- loadPackage fp
        di <- case mdi of
             Just di -> return di
             Nothing -> error $ "Could not parse cabal file: " ++ fp
        allGood <- checkDepsCli quiet excludes checkDeps newest di
        depsGood <- if deep
                       then do putStrLn $ "\nTransitive dependencies:"
                               allM (checkDepsCli quiet excludes checkLibDeps newest) (deepLibDeps newest [di])
                       else return True
        when (not (allGood && depsGood)) $ putStrLn ""
        return $ wasAllGood && allGood && depsGood

-- | Non short-circuiting monadic version of 'all'
-- Duh, pre-AMP code.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM f xs = and `liftM` mapM f xs
