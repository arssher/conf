#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-f]
    Configure and build Postgres, found in \$PGSDIR, with prefix
    (installation path) \$PGIDIR, using \$PGDIR as a build directory.
    Doesn't install it.
    By default it is a debug build.

    -h display this help and exit
    -f fullspeed: turn on optimizations, disable assers, etc
EOF
    exit 0
}

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

fullspeed=""
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "fh" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	f)
	    fullspeed=true
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

# I suppose I don't need this, but who knows...
# if ! [[ $PGDIR -ef $PGSDIR ]]; then
#     # vpath build, directories differ
#     rm -rf $PGDIR
# fi

mkdir -p $PGDIR
cd $PGDIR

if [ "$fullspeed" = true ]; then
    CFLAGS="-O3" $PGSDIR/configure --prefix=$PGIDIR
else
    CFLAGS="-O0" $PGSDIR/configure --prefix=$PGIDIR --enable-tap-tests --enable-cassert --enable-debug
fi

make clean && \
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
    make -j4 world && \
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-' && \
    make check
