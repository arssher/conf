#!/bin/bash

set -e

show_help() {
    cat << EOF
    Usage:
      bash ${0##*/} [path]

    In each directory under [path] (by default .), set modification time of each
    file so that order by mod time is the same as order by file name.

    Options:
      -h: display this help and exit
EOF
    exit 0
}

path='.'
action=$1
if [[ $# != 0 ]]; then
    if [[ $action == -h* ]];
    then
	show_help
    fi
    path=$1
fi

# will break if newlines in files
find "${path}" -type d | while read d; do
    echo "Processing $d"
    find "${d}" -type f | sort | while read f; do
	echo "Processing file $f"
	touch "${f}"
	sleep 1
    done
done
