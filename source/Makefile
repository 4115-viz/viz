#
# Viz Compiler Makefile
#

TARGETS = vizscanner vizparser ast test-file

.PHONY: default
default: $(TARGETS)

# build our current tester file
# following the format from 
.PHONY: test-file
test-file:
	ocamlbuild test.native

.PHONY: vizscanner
vizscanner:
	ocamlbuild vizscanner.native

.PHONY: vizparser
vizscanner:
	ocamlbuild vizparser.native

.PHONY: ast
vizscanner:
	ocamlbuild ast.native

# clean the dir of the executable
.PHONY: clean
clean:
	rm -rf *.native _build output.txt

# clean then build
.PHONY: all
all: clean default

# test the scanner
.PHONY: run-test 
run-test:
	./test.native < example.viz > output.txt