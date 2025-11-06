#!/bin/bash

if [ ! -d "../revdeps-src" ]; then
  echo "Run in a directory parallel to the revdeps-src directory" >&2
  exit 1
fi

export _R_CHECK_CRAN_INCOMING_=false
export _R_CHECK_CRAN_INCOMING_REMOTE_=false

find ../revdeps-src -name '*.tar.gz' -type f -print0 | parallel -0 -j "$(nproc)" R CMD check --as-cran
