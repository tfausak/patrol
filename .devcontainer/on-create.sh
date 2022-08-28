#! /usr/bin/env sh
set -o errexit -o xtrace

cabal update

if ! test -f cabal.project.local
then
  cabal configure --enable-tests --jobs
fi
