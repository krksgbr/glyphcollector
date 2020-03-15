#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

file=`pwd`/`find out -name *.zip`

basename=`basename $file`
if [[ $basename == *"darwin"*  ]]; then
	dirname=`dirname $file`
	newname=$dirname/${basename/darwin/macOS}
	mv $file $newname
	file=$newname
fi

tag=v`cat ./version`

./upload-github-release-asset.sh github_api_token=$GITHUB_API_TOKEN owner=krksgbr repo=glyphcollector tag=$tag filename=$file
