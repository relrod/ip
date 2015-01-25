#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:relrod/ip.git" "$f/ip.git"
cabal haddock
pushd "$f/ip.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/ip/* "$f/ip.git/"
pushd "$f/ip.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://relrod.github.io/ip/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
