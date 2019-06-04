#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

OS=`uname -s`


rm -rf ./backend/dist-newstyle
rm -rf ./dist
rm -rf ./out
mkdir dist

cd backend
cabal build glyphcollector
cd ..

hsExe=`find $(pwd)/backend/dist-newstyle -type f -name glyphcollector`

node ./collect-backend-deps.js --exe=$hsExe --outdir=`pwd`/dist

yarn

./node_modules/.bin/parcel build --public-url . src/index.html
./node_modules/.bin/electron-forge make
