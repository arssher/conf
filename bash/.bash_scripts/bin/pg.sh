#!/bin/bash

set -e

show_help() {
    cat <<EOF
    Usage: pg.sh [-c confpath] [-p start_port] [-v] [pgnum]...

    -v run postgres under valgrind

    For example, pg.sh 0 1
    Use this whenever you need some Postgreses to evaluate things
EOF
    exit 0
}

conf_path="${HOME}/tmp/postgresql.conf"
start_port=5432
use_valgrind="false"

OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "hc:p:v" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	c)
	    conf_path="${OPTARG}"
	    ;;
	p)
	    start_port=$OPTARG
	    ;;
	v)
	    use_valgrind="true"
	    ;;
    esac
done

echo "optind is $OPTIND"
# shift array of arguments up to OPTIND to get the rest
shift $(expr $OPTIND - 1)
echo "left args: " $@
echo "----"

if [[ -z "$@" ]]; then # pg num not specified
    pgnums=( 0 )
else
    pgnums=( "$@" )
fi
echo "runnings pgs: $pgnums"

# pkill -9 postgres || true
for pgnum in "${pgnums[@]}"; do
    echo "Running pgum ${pgnum}"
    port=$(($start_port + $pgnum))
    echo "Starting on port ${port}"
    # kill existing instance on this port
    existing_pid=$(ps aux | grep "[p]ostgres -p ${port}" | awk '{print $2}')
    echo "killing old pg with pid ${existing_pid}"
    kill -SIGQUIT $existing_pid || true
    rm -rf "/tmp/data${pgnum}"
    initdb -D "/tmp/data${pgnum}"
    cp "${conf_path}" "/tmp/data${pgnum}/postgresql.conf"
    rm -rf "/tmp/postgresql_${port}.log"
    # 'restart' to be not confused by previous instance, who might not completed
    # shutdown yet
    if [[ "${use_valgrind}" == "false" ]]; then
	pg_ctl -o "-p ${port}"  -D "/tmp/data${pgnum}" -l "/tmp/postgresql_${port}.log" restart
    else
	# options are mostly copied from buildfarm client code
	# track-origin tracks the origin of uninitialised values
	# read-var-info reads information about variable types and locations from DWARF3 debug info
	# num-callers is maximum number of entries shown in stack traces that identify program locations.
	valgrind --tool=memcheck --trace-children=yes --track-origins=yes \
		 --read-var-info=yes --num-callers=20 --leak-check=no \
		 --gen-suppressions=all --error-limit=no \
		 --suppressions="${PGSDIR}/src/tools/valgrind.supp" \
		 --error-markers=VALGRINDERROR-BEGIN,VALGRINDERROR-END \
		 pg_ctl -o "-p ${port}"  -D "/tmp/data${pgnum}" -l "/tmp/postgresql_${port}.log" restart
    fi
done
