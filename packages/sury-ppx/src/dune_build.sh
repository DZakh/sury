#!/bin/bash

echo "current directory: $(pwd)"

echo "dune format: $(file $(which dune))"

echo "start building with dune"
dune build