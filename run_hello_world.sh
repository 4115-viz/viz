#!/bin/bash

PWD=$(pwd)
echo "-------------------------------------"
echo "Running Hello World Tests from $PWD"
echo "-------------------------------------"
echo

echo "-------------------------------------"
echo "Abstract Syntax Tree"
dune exec -- vizc test/helloworld.viz -a    # abstract syntax tree
echo "-------------------------------------"
echo

echo "-------------------------------------"
echo "Semantic Abstract Syntax Tree"
dune exec -- vizc test/helloworld.viz -s    # semantically checked abstract syntax tree
echo "-------------------------------------"
echo

echo "-------------------------------------"
echo "Scanner Tokens"
dune exec -- vizc test/helloworld.viz -ts   # scan the tokens and send to stdout
echo "-------------------------------------"
echo