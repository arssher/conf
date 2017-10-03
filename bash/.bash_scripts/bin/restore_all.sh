#!/bin/bash

set -e

echo "Copying everything to the machine..."

script_dir=`dirname "$(readlink -f "$0")"`
cd "${script_dir}"

for restore_script in restore_*.sh; do
    "${script_dir}/${restore_script}"
done;
