#!/bin/bash

# This script can be run on any supported OS to create a binary .tar.gz
# bundle. For Windows, msysgit contains all of the pieces needed to run this
# script.

set -ex

OS=$1

if [ -z $OS ]
then
  echo "Usage: build.sh osname"
  exit 1
fi

pushd $(stack path --project-root)

# Make the staging directory
mkdir -p bundle/build/purerl

# Strip the binary, and copy it to the staging directory
if [ "$OS" = "win64" ]
then
  BIN="purs.exe"
else
  BIN="purs"
fi
FULL_BIN="$(stack path --local-install-root)/bin/$BIN"
if [ "$OS" != "win64" ]
then
  strip "$FULL_BIN"
fi
cp "$FULL_BIN" bundle/build/purerl

# Copy extra files to the staging directory
cp bundle/README         bundle/build/purerl/
cp LICENSE               bundle/build/purerl/
cp INSTALL.md            bundle/build/purerl/

stack ls dependencies >bundle/build/purerl/dependencies.txt

# Make the binary bundle
pushd bundle/build > /dev/null
tar -zcvf ../${OS}.tar.gz purerl
popd > /dev/null

# Calculate the SHA hash
if [ "$OS" = "win64" ]
then
  # msys/mingw does not include shasum. :(
  SHASUM="openssl dgst -sha1"
else
  SHASUM="shasum"
fi

$SHASUM bundle/${OS}.tar.gz > bundle/${OS}.sha

# Remove the staging directory
rm -r bundle/build

popd > /dev/null
