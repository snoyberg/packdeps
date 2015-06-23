#!/usr/bin/env bash

set -exu

TAG=$(git rev-parse --short HEAD)

(
rm -rf docker/app
mkdir -p docker/app
stack build
cp $(stack exec which packdeps) docker/app
strip docker/app/packdeps
cp packdeps.sh docker/app
cp -r config static docker/app
cd docker
docker build -t snoyberg/packdeps:$TAG .
docker push snoyberg/packdeps:$TAG
)
