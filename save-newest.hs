import Distribution.PackDeps

main :: IO ()
main = do
    newest <- loadNewest
    writeFile "newest" $ show newest
