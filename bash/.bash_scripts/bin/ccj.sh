#!/bin/bash

# generate compile_commands.json for clangd
./configure CFLAGS='-O0 -glldb -std=c11' CC=clang
bear -- make -j8 world
make maintainer-clean
