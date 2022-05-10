#!/usr/bin/env bash

# Installing Node and NPM for Linux and Mac
sudo apt update
sudo apt install nodejs
sudo apt install npm

# Installing Other Opam and Dependencies for Linux and Mac
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
sudo apt-get update -y
sudo apt-get install -y unzip
sudo apt-get install -y bubblewrap

# Initializing Opam and Installing Dune
opam init
opam install dune
eval $(opam env)

# Install LLVM
cd functions/viz
opam init --auto-setup --yes --disable-sandboxing
chmod +x viz.opam
opam install . --deps-only --yes
sudo apt install llvm
eval $(opam config env)
dune clean
dune build