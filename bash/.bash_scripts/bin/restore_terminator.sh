#!/bin/bash

set -e

echo "Copying terminator config from ${CONFPATH} to the machine..."

backup_path="${CONFPATH}/terminator"
mkdir -p ~/.config/terminator
cp ${backup_path}/config ~/.config/terminator/config
