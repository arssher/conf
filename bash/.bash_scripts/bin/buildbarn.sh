#!/bin/bash

# like buildfarm, but small and private...

# usage:
# branches="PGPRO11_PGPRO-2385 PGPRO10_PGPRO-2385 PGPRO9_6_PGPRO-2385 PGPRO9_5_PGPRO-2385" buildbarn.sh
# in pg dir
# apply_patch can be set to apply something beforehand
# set 'pull' to pull the branch

set -e
# set -o xtrace # print each line

pg_dir=$(pwd)

if [ -z "$branches" ]; then
    echo "branches not specified"
    exit 1
fi

for branch in $branches; do
    echo "Working on ${branch}"
    cd "${pg_dir}"
    git checkout "${branch}"
    
    if [ -n "${pull}" ]; then
	git pull
    fi

    if [ -n "${apply_patch}" ]; then
	git apply "${apply_patch}"
    fi
    
    build_dir="/tmp/tmpcheck"
    PGSDIR="${pg_dir}" PGBDIR="${build_dir}" postgres-build-full.sh
    cd "${build_dir}"
    make check
    cd contrib/test_decoding
    make check

    if [ -n "${apply_patch}" ]; then
	cd "${pg_dir}"
	git reset --hard HEAD
    fi
done
