### Build the Viz compiler files

```
make
```

### Run the scanner test
```
make run-test
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)--a list of strings for viz scanner (needs to be updated obviously) 
-  `vizscanner.mll`: scanner
-  `vizparser.mly`: parser

### Other files

- `test.ml`: top-level file to test and run the scanner
- `example.viz`: sample viz source code
- `output.txt`: this will be the outputted scanned tokens
