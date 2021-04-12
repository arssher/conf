#!/bin/bash

>netstat.log

while true
do
    date >> netstat.log
    sudo netstat -tpn | grep -E "(127.12.1|Recv-Q)" >> netstat.log
    sleep 2
done
