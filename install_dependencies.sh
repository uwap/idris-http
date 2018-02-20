#!/bin/bash

dependencies=("https://github.com/ziman/lightyear" "https://github.com/ziman/idris-bytes")
wd=$(pwd)

function checkSuccess {
  if [ $1 != 0 ]; then
    cleanup
    echo $2
    exit $1
  fi
}

function cleanup {
  echo "Cleaning up ..."
  cd $wd
  rm -rf .idris-http-install 
}

mkdir -p .idris-http-install
cd .idris-http-install

for dep in ${dependencies[*]}; do
  name=$(basename $dep)

  echo "Downloading $name ..."
  git clone "$dep"
  checkSuccess $? "Failed to download $name"
  cd "$name"

  echo "Building $name ..."
  idris --build *.ipkg
  checkSuccess $? "Failed to build $name"

  echo "Installing $name ..."
  idris --install *.ipkg
  checkSuccess $? "Failed to install $name"
  cd ../
done

cleanup
echo "*** Done: All dependencies are installed"
