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

# All test programs
cd test/programs
./script-test-programs
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
