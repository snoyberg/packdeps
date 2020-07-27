{-# LANGUAGE ViewPatterns #-}
import Control.Applicative (many, some)
import Control.Monad (forM_, foldM, when)
import Control.Monad (liftM)
import Data.Char (isSpace)
import Data.List (foldl')
import Distribution.PackDeps
import Distribution.Package (PackageIdentifier (pkgName, pkgVersion), PackageName, unPackageName)
import Distribution.Text (display)
import Distribution.Version (Version)
import qualified Distribution.InstalledPackageInfo as IPI
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

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
    output <- readProcess "ghc-pkg" ["dump"] ""
    ipis <- either (fail . show) (return . map snd) $
        traverse IPI.parseInstalledPackageInfo $
        map (encodeUtf8 . T.pack) $
        splitPkgs output
    return $ foldl' apply newest $ map toPackInfo ipis
  where
    apply :: Newest -> (PackageName, PackInfo) -> Newest
    apply m (k, v) = Map.insert k v m

    toPackInfo :: IPI.InstalledPackageInfo -> (PackageName, PackInfo)
    toPackInfo ipi = (pkgName $ IPI.sourcePackageId ipi, pinfo) where
        pinfo = PackInfo
            { piVersion = pkgVersion $ IPI.sourcePackageId ipi
            , piEpoch   = 0
            , piDesc    = Nothing
            }

    -- copied from from Cabal
    splitPkgs :: String -> [String]
    splitPkgs = checkEmpty . map unlines . splitWith ("---" ==) . lines
      where
        -- Handle the case of there being no packages at all.
        checkEmpty [s] | all isSpace s = []
        checkEmpty ss                  = ss

        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs = ys : case zs of
                           []   -> []
                           _:ws -> splitWith p ws
          where (ys,zs) = break p xs

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
