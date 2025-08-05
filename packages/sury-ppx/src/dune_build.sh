#!/bin/bash

echo "current directory: $(pwd)"

echo "dune format: $(file $(which dune))"
echo "dune ocamlc: $(file $(which ocamlc))"
echo "dune ocamlopt: $(file $(which ocamlopt))"

echo "start building with dune"
dune build