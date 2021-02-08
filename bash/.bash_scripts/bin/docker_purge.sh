#!/bin/bash

set -e

echo "Stopping all containers"
docker stop $(docker ps -a -q) || true
echo "Removing all containers"
docker rm $(docker ps -a -q) || true

echo "Removing all temporary images"
docker rmi -f $(docker images | tail -n +2 | grep -E -v "pgmm|alpine|pathman" | awk '{ print $3 }')

echo "Removing all networks not in use"
docker network prune --force
