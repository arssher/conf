#!/bin/bash

set -e

counter=0

# ./initdb.sh

while true
do
    pgbench --no-vacuum -f query_min.sql -c 4 --time 1800
    counter=$((counter+1))

    if grep -i -E -e 'more than once' /data/ars/tmpd/postgresql_5432.log; then
	echo "error found"
	echo "spinned ${counter} times"
	break
    fi
done
