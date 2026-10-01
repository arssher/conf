#!/bin/bash

set -e

echo "Copying git config to the machine..."

bashfiles_backup_path="${CONFPATH}/git"
cp -p ${bashfiles_backup_path}/.gitconfig ~/
