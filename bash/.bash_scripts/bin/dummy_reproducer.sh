#!/bin/bash

set -e

counter=0

function rmc {
    rm -rf /tmp/tmp*
    rm -rf /tmp/core*
    # testgres
    rm -rf /tmp/tgsn_*
    rm -rf /tmp/tgsb_*
    # stolon
    rm -rf /tmp/stolon*
}


while true
do
    rmc && pip uninstall -y testgres && python setup.py install
    >tmp.log
    export TEST_CASE=test_logical_replication
    python tests/test_simple.py >tmp.log 2>&1
    if grep -i -E -e 'FAIL: test_logical_replication' tmp.log; then
	exit
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
