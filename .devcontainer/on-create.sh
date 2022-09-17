#! /usr/bin/env sh
set -o errexit -o xtrace

cabal --ignore-project update

if ! test -f cabal.project.local
then
  cabal configure --disable-optimization --enable-tests --jobs --test-show-details direct
fi

cabal update
