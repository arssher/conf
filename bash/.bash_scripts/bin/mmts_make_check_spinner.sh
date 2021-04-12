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

make -C /tmp/pgpro/contrib/mmts/ stop
cd /tmp/pgpro/ && make install && cd contrib/mmts/ && make clean && make install

counter=0
while true
do
    rmc
    date
    make check
    ret=$?
    if grep -i -E -e 'terminated by' tmp_check/log/*.log; then
	break
    fi
    if [ $ret -ne 0 ]; then
	echo "make check failed"
	break
    fi
    counter=$((counter+1))
    echo "spinned ${counter} times"
done
