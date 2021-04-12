#!/bin/bash

set -e

counter=0

while true
do
    psql -p 5452 -f ~/tmp/tmp/tmp/tmp1.sql > /dev/null
    if grep -i -E -e 'error' /tmp/postgresql_5452.log; then
	echo "error found"
	echo "spinned ${counter} times"
       break
    fi

done
