#!/bin/bash

set -e

echo "Copying git config to the yandex disk..."

backup_path="${CONFPATH}/git"
cp ~/.gitconfig ~/.gitignore ${backup_path}
