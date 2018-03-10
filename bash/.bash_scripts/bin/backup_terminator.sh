#!/bin/bash

set -e

echo "Copying terminator config to ${CONFPATH}..."

backup_path="${CONFPATH}/terminator"
cp ~/.config/terminator/config ${backup_path}/config
