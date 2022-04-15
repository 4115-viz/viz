#!/bin/bash

PWD=$(pwd)
echo "-------------------------------------"
echo "running master shell script from $PWD"
echo "-------------------------------------"

# These need to be refactored, at end of project
#cd test/scanner
#BASH script-token-scanning.sh

# These need to be refactored, at end of project
#cd ../parser
#BASH script-parsing.sh

# These need to be refactored, at end of project
#cd ../semantic
#BASH script-semantic.sh

# build and run the test programs
#cd ../../
./script-test-programs.sh