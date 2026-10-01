#!/bin/bash

set -e

counter=0

while true
do
    cd /tmp/pgpro2 && make -j4 install
    cd ~/postgres/pg_pathman/ && USE_PGXS=1 make install
    pg.sh -p 5452 0

    psql -p 5452 -f ~/tmp/tmp/tmp/tmp11.sql

    counter=$((counter+1))

    if grep -i -E -e 'fault|problem in alloc set' /tmp/postgresql_5452.log; then
    # if grep -i -E -e 'fault|problem in alloc set|wrong constraint format for HASH partition' /tmp/postgresql_5452.log; then
    # if grep -i -E -e 'corrupt header in block' /tmp/postgresql_5452.log; then
    # if grep -i -E -e 'segmentation fault' /tmp/postgresql_5452.log; then
	echo "memctx error"
	echo "spinned ${counter} times"
       break
    fi
    # if grep -E -i -e 'valgrinderror' /tmp/postgresql_5452.log; then
	# echo "valgrind error found"
	# break
    # fi
done
