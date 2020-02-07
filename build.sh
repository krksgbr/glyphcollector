#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

OS=`uname -s`

if [[ "$OS" == *"MINGW"* ]]; then
   OS="WIN"
fi

BACKEND="$(pwd)/backend"


rm -rf ./dist
rm -rf ./out

# Build backend
if [[ "$OS" == "WIN" ]]; then
   ./windows/build-backend.sh $BACKEND
   HS_EXE=`find $BACKEND/.stack-work/dist -name glyphcollector.exe`
else 
   rm -rf ./backend/dist-newstyle

   cd backend
   cabal build glyphcollector
   cd ..

   HS_EXE=`find $(pwd)/backend/dist-newstyle -type f -name glyphcollector`

   node ./collect-backend-deps.js --exe=$HS_EXE --outdir=`pwd`/dist
fi

mkdir dist
cp $HS_EXE dist

yarn

./node_modules/.bin/parcel build --public-url . src/index.html
./node_modules/.bin/electron-forge make
