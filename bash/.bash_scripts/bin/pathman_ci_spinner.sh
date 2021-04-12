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

rm -rf t*.log tests.log
docker-compose build

counter=0
while true
do
    rmc
    date
    docker_purge.sh
    docker-compose run $(bash <(curl -s https://codecov.io/env)) tests >"t_${counter}.log"
    ret=$?
    if [ $ret -ne 0 ]; then
	echo "test failed"
	break
    fi
    container_id=$(docker ps -a | grep -i -E "pg_pathman_tests_run" | awk '{ print $1 }')
    rm -rf tests.log
    docker cp ${container_id}:/pg/testdir/tests/python/tests.log .
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
