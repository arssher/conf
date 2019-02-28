#!/bin/bash

set -e

counter=0

while true
do
    cd ~/tmp/tmp/ee11 && make -j4 install
    cd ~/postgres/pg_pathman/ && USE_PGXS=1 make install
    pg.sh -p 5442 0

    psql -p 5442 -f ~/tmp/tmp/tmp/tmp.sql &
    # make sure backend started and library loaded to dump proc maps
    while ! grep -e 'call for coverage test' /tmp/postgresql_5442.log
    do
	sleep 0.2
    done
    # backend_pid=$(psql -X -qAt -p 5442 -c "select pid from pg_stat_activity where application_name = 'psql' and pid != pg_backend_pid();" 2>/dev/null)
    # echo "backend_pid is ${backend_pid}"
    # if [ -z "${backend_pid}" ]; then
	# echo "backend pid not found"
	# exit 1
    # fi
    # cat "/proc/${backend_pid}/maps" > backend_maps.txt
    wait

    counter=$((counter+1))

    if grep -i -E -e 'fault|problem in alloc set' /tmp/postgresql_5442.log; then
    # if grep -i -E -e 'fault|problem in alloc set|wrong constraint format for HASH partition' /tmp/postgresql_5442.log; then
    # if grep -i -E -e 'corrupt header in block' /tmp/postgresql_5442.log; then
    # if grep -i -E -e 'segmentation fault' /tmp/postgresql_5442.log; then
	echo "memctx error"
	echo "spinned ${counter} times"
       break
    fi
    # if grep -E -i -e 'valgrinderror' /tmp/postgresql_5442.log; then
	# echo "valgrind error found"
	# break
    # fi
done
