#!/bin/bash

set -e

while read line
do
    if [[ ${line:0:1} == "{" ]] || [[ ${line:0:1} == "[" ]]; then
	echo "${line}" | json_pp
    else
	echo "${line}"
    fi
done < "${1:-/dev/stdin}"
