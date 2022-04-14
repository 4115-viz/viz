```
opam install ppx_expect ppx_inline_test
dune runtest
```

To test the expected output:
```
dune utop test/ppx
```

In the REPL:
```
Ppx__Ppx_ast.parse "func main(): int {}"
```