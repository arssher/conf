#!/bin/bash

set -e

function run_psql {
    psql -p 5442 -f ~/tmp/tmp/tmp/tmp.sql &
}

while true
do
    cd ~/tmp/tmp/ee11 && make -j4 install
    cd ~/postgres/pg_pathman/ && USE_PGXS=1 make clean install && pg.sh -p 5442 0
    # sleep 1

    run_psql &
    # let psql start before connecting
    sleep 1
    backend_pid=$(ps -eF | grep "[p]ostgres: $(whoami)" | awk '{print $2}')
    echo "backend_pid is ${backend_pid}"
    	# -ex 'b pl_comp.c:350 if strncmp(function->fn_signature, "drop_caches", strlen("drop_caches")) == 0' \
    gdb -p $backend_pid \
	-ex "trace_mem_ctx_violations" \
	-ex "c" \
	-ex "quit_if_alone"
    wait
    if grep -i -E -e 'fault|problem in alloc set' /tmp/postgresql_5442.log; then
	echo "mem problem found"

	break
    fi
done
