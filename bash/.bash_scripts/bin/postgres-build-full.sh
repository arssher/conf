#!/usr/bin/env bash
set -e

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh 

rm -rf $PGBDIR
mkdir -p $PGBDIR
cd $PGBDIR
CFLAGS="-O0" $PGDIR/configure --prefix=$PGIDIR --enable-tap-tests --enable-cassert --enable-debug && \
  make clean && \
  echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
  make -j4 world && \
  echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
  make check
