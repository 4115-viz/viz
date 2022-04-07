# Viz Programming Language

## Getting Started
### Environment Setup

### Building
#### Build the Viz compiler files

```
dune clean
dune build
```

#### Run the scanner test
```
# from /viz directory
dune exec -- vc test/helloworld.viz -a    /* abstract syntax tree */
dune exec -- vc test/helloworld.viz -s    /* semantically checked abstract syntax tree */
dune exec -- vc test/helloworld.viz -ts   /* scan the tokens and send to stdout */
```

#### Run the automated test scripts
```
# hello world test file
./run_hello_world.sh

# run the whole test suite
./run_viz_tests.sh

# scanner test files
cd test/scanner
./script-token-parsing.sh

# parser test files
TBD

# other test files
TBD
```
<!-- #### Compiler files
-  `ast.ml`: abstract syntax tree (AST)--a list of strings for viz scanner (needs to be updated obviously) 
-  `scanner.mll`: scanner
-  `parser.mly`: parser -->

<!-- #### Other files -->
<!-- - `test.ml`: top-level file to test and run the scanner -->
<!-- - `example.viz`: sample viz source code -->
<!-- - `output.txt`: this will be the outputted scanned tokens -->
