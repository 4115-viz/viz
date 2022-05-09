# Viz Programming Language

## Getting Started
### Environment Setup
```
Need to ensure that you have the following software installed.
ocaml = 4.13.1 (group lets discuss...)
dune >= 3.0
llvm = 11.0.0 or Docker installed to build our container image with LLVM installed (see below)
```

# Why is Viz Docker Image Needed?
Either the user could have LLVM locally installed (kinda complicated) or install and run
our Docker container (prepackaged with LLVM). We need LLVM in order to compile our code
to IR, and ultimately to build and run the executable. Follow the below instructions

# Build Viz Docker Image
Make sure that you have Docker Desktop downloaded and running, and then use the following 
commands to build our viz container. 

# Make Sure You Have Docker Installed and Running

```
# build our container
docker build -t viz .

# if you see the below error, when building the viz containen then rune the below command
if "ERROR [7/9] COPY ./viz.opam ." upon docker build 

dune build
```

# At this point, the container image should be downloaded
#### To use the bash of the docker container:

1. Start the container if it's not running.
```
docker start <container_name>
```

2. launch the container
```
This command will place you inside the container at the root, and you can run any
of the tests in the next section without fear of software dependency issues.

# from /viz root directory
./launch-container.sh
```

## Building the Code
At this point you should have built the Viz Docker Image, and be inside the container. That
will ensure no issues with running any of the following commands.

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
dune exec -- vc <program-name>.viz  -ts   /* scan tokens and print to stdout */
dune exec -- vc <program-name>.viz  -a    /* abstract syntax tree */
dune exec -- vc <program-name>.viz  -s    /* semantically checked abstract syntax tree */
dune exec -- vc <program-name>.viz  -l    /* LLVM IR */

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

#### Run the Full Automated Test Suite (Recommended)
```
This shell script will run all of the tests across all of the test/* 
directories.
# from /viz root directory
./run_viz_tests.sh
```

#### Compile and Run a Singular Test program
```
./viz test/programs/helloworld.viz
```

## VizOnline Text Editor

We have implemented an online text editor for our language in order to provide Viz 
programming language users a more interactive way of developing code. With this deployed 
web application, the user doesn’t have to worry about having Dune, Ocaml, and LLVM installed 
locally on their machine. VizOnline affords users the ability to write, compile, and run viz 
source code in an online text editor. Furthermore, we provide a look into what happens under 
the hood during compilation through 6 different run options: 

  1) Running the code and displaying output
  2) A look at the full build process
  3) Displaying the parsed abstract syntax tree
  4) Presenting the syntactically checked abstract syntax tree
  5) Viewing the LLVM IR assembly code
  6) Seeing how programs are scanned into tokens

#### Web Hosted Text Editor
```
http://ec2-23-22-206-12.compute-1.amazonaws.com/
```

Unfortunately, due to server costs, we are unable to keep the hosted version online permanently. 
Therefore, we have included the source code for both the frontend and backend of the web interface 
in our project repository under the web-interface folder.

In order to run the project locally, it requires that the machine has both node and npm installed 
in addition to opam, dune, and llvm which are required to compile and run the actual code. If you 
do not already have node installed, you can do so by a tutorial that can be found here. After you 
have both node and npm installed you can navigate into the ./web-interface directory of the Viz 
GitHub Repository and run the below commands to start both the backend and frontend servers locally.

Local Run Commands:

```
# Start Backend Server
node server.js

# Start Frontend Server
cd client
npm start
```
