#!/bin/bash -ex

cabal clean
cabal configure -fweb
cabal build
rm -f packdeps-yesod.bz2
cp dist/build/packdeps-yesod/packdeps-yesod .
bzip2 packdeps-yesod
scp packdeps-yesod.bz2 ubuntu@packdeps.haskellers.com:/home/ubuntu/packdeps
ssh ubuntu@packdeps.haskellers.com 'cd packdeps && mv packdeps-yesod packdeps-yesod.old && bunzip2 packdeps-yesod.bz2 && ./packdeps-yesod --save-newest && sudo killall packdeps-yesod'
