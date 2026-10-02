#!/bin/sh

pids=$()

for pid in $(seq 2 150)
do
    # numheld=$(gdb -p ${pid} -nx -batch -ex "print num_held_lwlocks")
    numheld=$(gdb -p ${pid} -nx -batch -ex "print pb_preparers_incremented" 2>/dev/null)
    echo "pid=${pid} numheld=${numheld}"
done
