#!/bin/bash

counter=0

while true
do
    echo "junk commit num ${counter}" >> README.md
    git add README.md
    git commit -m "junk commit num ${counter}"
    git push mk postgresql-13-port
    counter=$((counter+1))
    echo "committed ${counter} times"
    sleep 90m
done
