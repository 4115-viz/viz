#
# Viz Compiler Makefile
#

TARGETS = scanner parser ast

.PHONY: default
default: $(TARGETS)

# build our current tester file
# following the format from 
# .PHONY: test-file
# test-file:
# 	ocamlbuild test.native

.PHONY: scanner
vizscanner:
	ocamlbuild scanner.native

.PHONY: parser
vizscanner:
	ocamlbuild parser.native

.PHONY: ast
vizscanner:
	ocamlbuild ast.native

# clean the dir of the executable
.PHONY: clean
clean:
	dune clean

# clean then build
.PHONY: all
all: clean default

# test the scanner
# .PHONY: run-test 
# run-test:
# 	./test.native < example.viz > output.txt