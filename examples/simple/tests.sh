#!/usr/bin/env bash

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'
CWD="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"

passes=0
fails=0

function fail {
  echo -e "${RED}$1 ($2)$NC"
  ((fails++))
}

function success {
  echo -e "${GREEN}$1 ($2)$NC"
  ((passes++))
}

function pattern_in_resp {
  tmp="$(mktemp)"
  cmd="$CWD/simple $1 $2 $3"
  $cmd > $tmp
  if [[ $? -ne 0 ]]; then
    fail "simple failed to run" "$cmd"
    return 1
  else
    grep -q "$4" $tmp
    if [[ $? -ne 0 ]]; then
      fail "output missing pattern ($4)" "$cmd"
      rm $tmp
      return 1
    else
      success "matches pattern ($4)" "$cmd"
      rm $tmp
      return 0
    fi
  fi
}

pattern_in_resp 4.da.gd 80 /ip '\\.'
pattern_in_resp www.google.com 80 / 'Feeling Lucky'
pattern_in_resp restify.com 80 / 'Production Ready'

echo "$passes passes and $fails fails"
exit $fails
