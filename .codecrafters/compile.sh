#!/bin/sh
#
# This script is used to compile your program on CodeCrafters
#
# This runs before .codecrafters/run.sh
#
# Learn more: https://codecrafters.io/program-interface

set -e # Exit on failure

env
opam list
opam install base
opam list
dune build --build-dir /tmp/codecrafters-build-interpreter-ocaml
