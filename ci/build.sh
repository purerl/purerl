#!/bin/bash

set -ex

# This is the main CI build script. It is intended to run on all platforms we
# run CI on: linux, mac os, and windows. It makes use of the following
# environment variables:
#
# - CI_RELEASE
#
#   If set to "true", passes the RELEASE flag to the compiler, and enables
#   optimizations. Otherwise, we disable optimizations (to speed builds up).
#
# = Source distributions
#
# During a normal build, we create a source distribution with `stack sdist`,
# and then compile and run tests inside that. The reason for this is that it
# helps catch issues arising from forgetting to list files which are necessary
# for compilation or for tests in our package.yaml file (these sorts of issues
# don't test to get noticed until after releasing otherwise).

(echo "::group::Initialize variables") 2>/dev/null

STACK="stack --no-terminal --jobs=2"

# Disable until deps are set up
# STACK_OPTS="--test"
if [ "$CI_RELEASE" = "true" ]
then
  STACK_OPTS="$STACK_OPTS --flag=purerl:RELEASE"
else
  STACK_OPTS="$STACK_OPTS --fast"
fi

(echo "::endgroup::"; echo "::group::Install snapshot dependencies") 2>/dev/null

# Install snapshot dependencies (since these will be cached globally and thus
# can be reused during the sdist build step)
$STACK build --only-snapshot $STACK_OPTS

(echo "::endgroup::"; echo "::group::Build source distributions") 2>/dev/null

# Test in a source distribution (see above)
$STACK sdist --tar-dir sdist-test;
tar -xzf sdist-test/purerl-*.tar.gz -C sdist-test --strip-components=1

(echo "::endgroup::"; echo "::group::Build and test purerl") 2>/dev/null

pushd sdist-test
$STACK build --pedantic $STACK_OPTS
popd

(echo "::endgroup::") 2>/dev/null