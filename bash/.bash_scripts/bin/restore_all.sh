#!/bin/bash

set -e

echo "Copying everything to the machine..."

script_dir=`dirname "$(readlink -f "$0")"`
cd "${script_dir}"

for restore_script in restore_*.sh; do
    if [ "${restore_script}" != "restore_all.sh" ]; then
	"${script_dir}/${restore_script}"
    fi;
done;
