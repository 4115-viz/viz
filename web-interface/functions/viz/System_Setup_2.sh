#!/usr/bin/env bash

# Install LLVM
cd web-interface/functions/viz
opam init --auto-setup --yes --disable-sandboxing
chmod +x viz.opam
opam install . --deps-only --yes
sudo apt install llvm
eval $(opam config env)
dune clean
dune build