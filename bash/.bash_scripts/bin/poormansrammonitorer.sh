#!/bin/bash

echo $(date) $(free -h | head -n 1) > mem.txt

while true
do
    echo $(date) $(free -h | head -n 2 | tail -n 1) >> mem.txt
    sleep 1
done
