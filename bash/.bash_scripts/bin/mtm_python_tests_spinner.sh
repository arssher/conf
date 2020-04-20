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

counter=0
success=0

while true
do
    rmc
    cd ~/postgres/pgpro/contrib/mmts/tests
    docker_purge.sh
    mtm_clean_logs.sh
    cd /tmp/pgpro/contrib/mmts/
    make clean
    make
    cd ~/postgres/pgpro/contrib/mmts/tests
    python -u test_recovery_random.py --failfast > "t_${counter}.log" 2>&1
    retc=$?
    mtm_collect_logs.sh

    if [ $retc -ne 0 ]; then
	echo "TEST FAILED"
	break;
    else
	success=$((success+1))
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times, successfull ${success} of them"
done
