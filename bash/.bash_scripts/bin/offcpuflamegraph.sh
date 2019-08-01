#!/bin/bash

set -e

name=$1

flamegraph.pl --color=aqua --countname=us --title="Off-CPU" < "${name}.offcpustacks" > "${name}_offcpu.svg"
