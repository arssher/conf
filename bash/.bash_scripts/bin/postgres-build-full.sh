#!/usr/bin/env bash
set -e

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh 


if ! [[ $PGDIR -ef $PGSDIR ]]; then
    # vpath build, directories differ
    rm -rf $PGDIR
fi

mkdir -p $PGDIR
cd $PGDIR
CFLAGS="-O0" $PGSDIR/configure --prefix=$PGIDIR --enable-tap-tests --enable-cassert --enable-debug && \
  make clean && \
  echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
  make -j4 world && \
  echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
  make check
