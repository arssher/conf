#!/bin/bash

set -e

# remove all containers
docker stop $(docker ps -a -q) || true
docker rm $(docker ps -a -q) || true

# and all temporary images
docker rmi -f $(docker images | tail -n +2 | grep -E -v "pgproee|alpine" | awk '{ print $3 }')

# and all unused networks
docker network prune --force
