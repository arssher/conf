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

mmpath="${HOME}/postgres/mmts"
# mmpath="${HOME}/postgres/pgpro/contrib/mmts"

counter=0

cd "${mmpath}/tests"
rm t_*.log

while true
do
    # rmc
    date

    echo "running test_recovery_random"
    docker_purge.sh
    mtm_clean_logs.sh
    python -u test_recovery_random.py --failfast > "t_rec_${counter}.log" 2>&1
    retc=$?
    mtm_collect_logs.sh
    if [ $retc -ne 0 ]; then
	echo "TEST FAILED"
	break;
    fi

    echo "running test_referee"
    docker_purge.sh
    mtm_clean_logs.sh
    python -u test_referee.py --failfast > "t_ref_${counter}.log" 2>&1
    retc=$?
    mtm_collect_logs.sh
    if [ $retc -ne 0 ]; then
	echo "TEST FAILED"
	break;
    fi

    echo "running test_syncpoint"
    docker_purge.sh
    mtm_clean_logs.sh
    python -u test_syncpoint.py --failfast > "t_sp_${counter}.log" 2>&1
    retc=$?
    mtm_collect_logs.sh
    if [ $retc -ne 0 ]; then
	echo "TEST FAILED"
	break;
    fi

    counter=$((counter+1))
    echo "successfully spinned ${counter} times"
done
