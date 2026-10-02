#!/bin/bash

# set -e

counter=0

while true
do
    pkill -9 postgres; rm -rf /dev/shm/PostgreSQL.*; cd /tmp/pgpro2 && make install && cd contrib/mmts/ && make clean && make && make check
    if grep -i -E -e 'deadlock detected' /home/ars/postgres/pgpro2/contrib/mmts/results/regression.diffs; then
	exit
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
