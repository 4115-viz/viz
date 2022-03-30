# Viz Programming Language

## Introduction
Viz is a general-purpose programming language that allows the visualization of data structure and the highlight of each operation step.  It is imperative, statically scoped, statically and weakly typed like C++ but with simpler features and more intuitive syntax.  Our programming language supports the most basic primitive data types, operations, and control flows.  On top of the basics, we also include features such as garbage collection, object-oriented and abstract data types like array, stack, and tree.  For visualization, Viz will generate a styled HTML file for developers to examine each step of operation applied to data structures. Furthermore, viz will be a helpful tool for beginners through friendly syntax which will allow users to not get bogged down by computer science fundamentals, but rather focus on writing code that executes a given task.
## Getting Started
### Environment Setup
```
git clone git@github.com:4115-viz/viz.git
cd viz

We use dune to build and run our ocaml code.  Dune 3.0.3 required to run our code.  If you do not have it follow the steps here: https://ocaml.org/learn/tutorials/up_and_running.html

```
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

## Lexical structure

### Keywords
The following keywords are reserved and may not be used as identifiers.

`func`, `if`, `else`, `return`, `for`

### Comments
Comments are treated as whitespace.
1. **Line Comments:** `//`
2. **Multiline Comments:** begin with `/*` and end with `*/`

### Operators
- Artithmetic: `+`, `-`, `*`, `/`, `%`
- Assignment: `=`, `+=`, `-=`, `*=`, `%=`
- Comparison: `==`, `!=`, `>=`, `<=`, `>`, `<`
- Logical: `and`, `or`, `not`

## Types
- Primitive types:
  - **Boolean:** - `true` or `false`
  - **String:** - the set of string values, ex. "abc"
  - **Int:**
  - **Float:**
  - **None:** A type with no values 
- Complex Data Types:
  - Array
  - Queue
  - Stack
  - Doubly linked list
  - Binary Tree

## Statements
### Loop Statement
```
```
### If Statement
`if` statement execute code based on conditions
```
if `condition` {
  `statements`
}
```

### Return Statement
```
return
return `expression`
```

## Declaration