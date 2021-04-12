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

cd ~/tmp/postgresql && make

counter=0
while true
do
    rmc
    date
    make installcheck
    ret=$?
    if [ $ret -ne 0 ]; then
	echo "make check failed"
	break
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
