#!/bin/bash

PWD=$(pwd)
echo "-------------------------------------"
echo "running master shell script from $PWD"
echo "-------------------------------------"

cd test/scanner
BASH script-token-scanning.sh

cd ../parser
BASH script-parsing.sh

#cd ../semantic
#BASH script-semantic.sh

cd ../programs
BASH script-test-programs.sh