#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-t target]

    Run 'make' from \$PGBDIR directory to build Postgres. It must be configured,
    e.g. postgres-build-full.sh run first.

    -h display this help and exit
EOF
    exit 0
}

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

target="all"

while getopts ":h" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
	t)
	    target=$OPTARG
	    ;;
    esac
done

cd $PGBDIR
echo $PGBDIR
# exit
make -j4 "${target}"
