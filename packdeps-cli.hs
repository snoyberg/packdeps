import Distribution.PackDeps
import System.Environment (getArgs)
import Distribution.Text (display)
import Distribution.Package (PackageName (PackageName))

main :: IO ()
main = do
    newest <- loadNewest
    getArgs >>= mapM_ (go newest)
  where
    go newest fp = do
        mdi <- loadPackage fp
        di <-
            case mdi of
                Just di -> return di
                Nothing -> error $ "Could not parse cabal file: " ++ fp
        case checkDeps newest di of
            (pn, v, AllNewest) ->
                putStrLn $ concat
                    [ prettyN pn
                    , "-"
                    , prettyV v
                    , ": Can use newest versions of all dependencies"
                    ]
            (pn, v, WontAccept p _) -> do
                putStrLn $ concat
                    [ prettyN pn
                    , "-"
                    , prettyV v
                    , ": Cannot accept the following packages"
                    ]
                flip mapM_ p $ \(k, v) -> putStrLn $ k ++ " " ++ v
        putStrLn ""

prettyN (PackageName n) = n
prettyV = display
