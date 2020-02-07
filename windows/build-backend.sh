#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

DIR=$1
cd $DIR
stack build
