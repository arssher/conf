#!/bin/bash

set -e

echo "Copying readline config to the yandex disk..."

backup_path="${CONFPATH}/readline"
cp ~/.inputrc ${backup_path}
