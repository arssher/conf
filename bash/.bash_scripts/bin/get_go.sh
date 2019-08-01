#!/bin/bash

set -e

# golink="https://dl.google.com/go/go1.11.10.linux-amd64.tar.gz"
golink="https://dl.google.com/go/go1.12.6.linux-amd64.tar.gz"
goarchive=$(basename -- "${golink}")

echo "Downloading go from ${golink}..."
wget $golink
rm -rf ~/opt/go
tar -xvzf $goarchive -C ~/opt/
rm $goarchive
