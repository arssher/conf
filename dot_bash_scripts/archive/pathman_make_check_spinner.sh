#!/bin/bash

function rmc {
    rm -rf /tmp/tmp*
    rm -rf /tmp/core*
    # testgres
    rm -rf /tmp/tgsn_*
    rm -rf /tmp/tgsb_*
    # stolon
    rm -rf /tmp/stolon*
}

rmc && cd /tmp/pgpro4/ && make install && cd ~/postgres/pg_pathman/ &&
    USE_PGXS=1 make clean install && pg.sh -p 5452 0 && sleep 2

counter=0
while true
do
    rmc
    date
    USE_PGXS=1 PGPORT=5452 make installcheck
    ret=$?
    if [ $ret -ne 0 ]; then
	echo "make check failed"
	break
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
