#!/bin/bash

./configure CFLAGS='-O0 -glldb -std=c11' CC=clang
bear -- make -j8
