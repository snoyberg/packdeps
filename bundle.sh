#!/bin/bash -ex

cabal build
strip dist/build/packdeps/packdeps
rm -rf static/tmp
tar czfv packdeps.keter dist/build/packdeps/packdeps config static
