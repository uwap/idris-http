#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone "git@github.com:uwap/idris-http.git" "$f/idris-http.git"
idris --mkdoc http.ipkg
pushd "$f/idris-http.git"
  git checkout gh-pages && git rm -rf *
popd
mv http_doc/* "$f/idris-http.git/"
pushd "$f/idris-http.git"
  git add -A
  git commit -m "Manual docs deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://uwap.github.io/idris-http/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
