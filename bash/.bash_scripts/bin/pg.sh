#!/bin/bash

set -e

show_help() {
    cat <<EOF
    Usage: pg.sh [-c confpath] [pgnum]...
    For example, pg.sh 0 1
    Use this whenever you need some Postgreses to evaluate things
EOF
    exit 0
}

conf_path="${HOME}/tmp/postgresql.conf"

OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "hc:" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	c)
	    conf_path="${OPTARG}"
    esac
done

# shift array of arguments up to OPTIND to get the rest
shift $(expr $OPTIND - 1)
echo $@
echo "----"

if [[ -z "$@" ]]; then # pg num not specified
    pgnums=( 0 )
else
    pgnums=( "$@" )
fi

# pkill -9 postgres || true
for pgnum in "${pgnums[@]}"; do
    echo "Running pgum ${pgnum}"
    port=$((5432 + $pgnum))
    # kill existing instance on this port
    existing_pid=$(ps aux | grep "[p]ostgres -p ${port}" | awk '{print $2}')
    kill -SIGQUIT $existing_pid || true
    rm -rf "/tmp/data${pgnum}"
    initdb -D "/tmp/data${pgnum}"
    cp "${conf_path}" "/tmp/data${pgnum}/postgresql.conf"
    rm -rf "/tmp/postgresql_${port}.log"
    pg_ctl -o "-p ${port}"  -D "/tmp/data${pgnum}" -l "/tmp/postgresql_${port}.log" restart
done
