#!/bin/bash

PWD=$(pwd)
echo "-------------------------------------"
echo "running master shell script from $PWD"
echo "-------------------------------------"

# run scanner tests
cd test/scanner
BASH script-token-scanning.sh

# run parser tests
cd ../parser
BASH script-parsing.sh

# run semantic tests
cd ../semantic
BASH script-semantic.sh

# change back to root + build and run the test programs
cd ../../
./script-test-programs.sh