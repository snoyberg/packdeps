#!/bin/bash -ex

cabal clean
cabal configure -fweb
cabal build
rm -f packdeps-yesod.bz2 save-newest.bz2
cp dist/build/packdeps-yesod/packdeps-yesod dist/build/save-newest/save-newest .
bzip2 packdeps-yesod
bzip2 save-newest
scp packdeps-yesod.bz2 save-newest.bz2 ubuntu@packdeps.haskellers.com:/home/ubuntu/packdeps
ssh ubuntu@packdeps.haskellers.com 'cd packdeps && mv packdeps-yesod packdeps-yesod.old && bunzip2 packdeps-yesod.bz2 && mv save-newest save-newest.old && bunzip2 save-newest.bz2 && ./save-newest && sudo killall packdeps-yesod'
