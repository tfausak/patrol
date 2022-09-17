#! /usr/bin/env sh
set -o errexit -o xtrace

if ! command -v "$1" >&2
then
  cabal --ignore-project install "$1" >&2
fi

exec "$@"
