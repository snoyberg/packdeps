import CheckDeps

main = do
    newest <- loadNewest "/home/snoyman/.cabal/packages/hackage.haskell.org/00-index.tar"
    writeFile "newest" $ show newest
