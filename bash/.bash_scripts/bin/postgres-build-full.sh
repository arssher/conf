#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-t target] [-f] [-r]

    Configure and build Postgres, found in \$PGSDIR, with prefix
    (installation path) \$PGIDIR, using \$PGDIR as a build directory.
    Doesn't install it.
    By default it is a debug build.

    -h display this help and exit
    -t target
       make target, for example, 'world'. By default it is empty.
    -f fullspeed, release build: turn on optimizations, disable asserts, etc.
    -r run regression tests. By default they are not run.
EOF
    exit 0
}


script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

fullspeed=""
run_tests=""
target=""
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "fht:" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	f)
	    fullspeed=true
	    ;;
	r)
	    run_tests=true
	    ;;
	t)
	    target=$OPTARG
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

# Clean old installation, if it exists
# Without that, build might fail or old setting might slip through
if [ -d "$PGDIR" ]; then
    # || true because it will fail if it is a clean postgres (configure was never
    # run)
    cd $PGDIR && make distclean || true
fi

mkdir -p $PGDIR
cd $PGDIR

# run configure
if [ "$fullspeed" = true ]; then
    CFLAGS="-O2" "$PGSDIR/configure" --prefix="$PGIDIR"
else
    CFLAGS="-O1" "$PGSDIR/configure" --prefix="$PGIDIR" --enable-tap-tests --enable-cassert --enable-debug
fi

# run make
echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
if [ -z "$target" ]; then
    make -j4
else
    make -j4 "$target"
fi
echo "Postgres at $PGSDIR successfully built"

# run tests, if needed
if [ "$run_tests" = true ]; then
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
    make check
fi
