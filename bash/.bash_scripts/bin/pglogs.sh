#!/bin/bash

script_dir=`dirname "$(readlink -f "$0")"`

args=("$@") # turn args into array
if [ $# = 1 ]; then
    log="${args[0]}"
else
    log=/tmp/shmn.log
fi

pcregrep -M -f "${script_dir}/patterns.txt"  "${log}"
