#!/bin/bash

set -e

show_help() {
    cat <<EOF
    Usage: pg.sh [pgnum] ...
    For example, pg.sh 0 1
    Use this whenever you need some Postgreses to evaluate things
EOF
    exit 0
}

OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "m:ht:s" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
    esac
done

if [ -z $1 ]; then # pg num not specified
    pgnums=( 0 )
else
    pgnums=( "$@" )
fi

pkill -9 postgres || true
for pgnum in "${pgnums[@]}"; do
    rm -rf "/tmp/data${pgnum}"
    initdb -D "/tmp/data${pgnum}"
    echo "shared_preload_libraries='pg_pathman'" >> "/tmp/data${pgnum}/postgresql.conf"
    echo "wal_level=logical" >> "/tmp/data${pgnum}/postgresql.conf"
    port=$((5432 + $pgnum))
    pg_ctl -o "-p ${port}"  -D "/tmp/data${pgnum}" -l /tmp/shmn.log restart
done
