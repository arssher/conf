#!/bin/bash

set -e

docker rmi pgproee11 ||
cd ~/postgres/pgpro && docker build -t pgproee11 .
