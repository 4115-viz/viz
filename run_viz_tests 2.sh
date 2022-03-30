#!/bin/bash

PWD=$(pwd)
echo "-------------------------------------"
echo "running master shell script from $PWD"
echo "-------------------------------------"

cd test/scanner
BASH script-token-parsing.sh