#!/bin/bash

# author: Crystal Ren
set -e

if [ -z "$1" ]
  then
    echo "Usage: ./run.sh <name.viz>"
    exit 1
fi

f="test/$1"
eval "dune exec vc $f"
