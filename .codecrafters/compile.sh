#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

env
echo ">>> before"
opam list
echo ">>> install"
opam install ocaml_intrinsics_kernel sexplib0 base
echo ">>> after"
opam list
echo ">>> compile"
dune build --build-dir /tmp/codecrafters-build-interpreter-ocaml
