#!/bin/sh

# Install ghc-tags-plugin for all installed GHC versions, assuming a GHCUP
# installation.

set -eu

installed=$(ghcup --offline list --tool ghc --show-criteria installed --raw-format)
versions=$(printf '%s\n' "$installed" | awk '$1 == "ghc" { print $2 }' | grep -v javascript | grep -v wasm32)

if [ -z "$versions" ]; then
    echo "No GHC versions are installed with ghcup." >&2
    exit 1
fi

for version in $versions; do
    printf 'Installing ghc-tags-plugin for GHC %s\n' "$version"
    make install GHC_VERSION="$version"
done
