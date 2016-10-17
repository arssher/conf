#!/usr/bin/env bash
set -e

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

cd $PGBDIR
make -j4
