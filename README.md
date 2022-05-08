# Viz Programming Language

## Getting Started
### Environment Setup
```
Need to ensure that you have the following software installed.
ocaml = 4.13.1 (group lets discuss...)
dune >= 3.0
llvm = 11.0.0 or Docker installed to build our container image with LLVM installed (see below)
```

## Building the Code
#### Build the Viz compiler files

```
dune clean /* clean the Dune _build folder */
dune build /* build our compiler code */
```

## Running Viz Test Suite

#### Compiling an Individual .viz Program at a Particular Build Stage
```
We leverage the Dune Build system to compile our program with different flags
and show the corresponding output. Check viz/bin/vc.ml for more details.

# from /viz root directory
dune exec -- vc <program-name>.viz  -ts    /* scan tokens and print to stdout */
dune exec -- vc <program-name>.viz  -a    /* abstract syntax tree */
dune exec -- vc <program-name>.viz  -s    /* abstract syntax tree */
dune exec -- vc <program-name>.viz  -l    /* abstract syntax tree */

```

#### Run Our Scanner Test Suite
```
# from /viz root directory
cd test/scanner
./script-token-scanning.sh
```

#### Run Our Parser Test Suite
```
# from /viz root directory
cd test/parser
./script-parsing.sh
```

#### Run Our Semantic Test Suite
```
# from /viz root directory
cd test/semantic
./script-semantic.sh
```

#### Compile and Run Test Programs
```
# from /viz root directory
./script-test-programs.sh
```

#### Run the Full Automated Test Suite
```
This shell script will run all of the tests across all of the test/* 
directories.
# from /viz root directory
./run_viz_tests.sh
```

#### How to compile and run programs!
If you do not manually build then it will hang when you run the ./vizDocker script
run this below command first, then ./vizDocker will used cached version.
# build viz docker image
```
docker build -t viz .
```
if "ERROR [7/9] COPY ./viz.opam ." upon docker build 
run dune build
```
dune build
```

# make sure you have docker installed and running

# To use the bash of the docker container:
1. If you haven't created the container yet, (skip if you have done this):
Make sure you are on the project's root directory.
```
docker run -it -v $(pwd):/home/viz -w=/home/viz viz /bin/bash
```
2. Start the container if it's not running.
```
docker start <container_name>
```
3. Run the bash shell of the container.
```
docker exec -it <container_name> /bin/bash 
```


# run a test program
`./vizDocker test/programs/helloworld.viz`

# run all test programs
```
cd test/programs
./script-test-programs

<!-- #### Compiler files
-  `ast.ml`: abstract syntax tree (AST)--a list of strings for viz scanner (needs to be updated obviously) 
-  `scanner.mll`: scanner
-  `parser.mly`: parser -->

<!-- #### Other files -->
<!-- - `test.ml`: top-level file to test and run the scanner -->
<!-- - `example.viz`: sample viz source code -->
<!-- - `output.txt`: this will be the outputted scanned tokens -->
