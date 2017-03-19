#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-t target] [-f] [-r]

    Configure and build Postgres, found in \$PGSDIR, with prefix
    (installation path) \$PGIDIR/\$PGINAME, using \$PGBDIR as a build directory.
    Then install it.

    -h display this help and exit
    -t target
       make target, for example, 'world'. By default it is empty.
    -m <d[ebug] | r[elease] | p[erf]>, 3 choices:
       debug: no optimizations, asserts, etc, default value
       release, turn on optimizations, disable asserts, etc.
       perf: like release, but with frame pointers not omitted to get callstacks
         on old perfs
    -r run regression tests. By default they are not run.
    -s silent make
EOF
    exit 0
}


script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

mode="debug"
run_tests=""
target=""
silent=""
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "m:ht:s" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	m)
	    mode=$OPTARG
	    ;;
	r)
	    run_tests=true
	    ;;
	t)
	    target=$OPTARG
	    ;;
	s)
	    silent="-s"
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

# Clean old installation, if it exists
# Without that, build might fail or old setting might slip through
if [ -d "$PGBDIR" ]; then
    # || true because it will fail if it is a clean postgres (configure was never
    # run)
    cd $PGBDIR && make distclean || true
fi

mkdir -p $PGBDIR
cd $PGBDIR

# run configure
# opts for proper inlining
CFLAGS="${CFLAGS} -std=c99 -Wno-unused-function"
CFLAGS="${CFLAGS} --param large-stack-frame=4096 --param large-stack-frame-growth=100000"
    # since debug symbols doesn't affect perfomance, include them in rel mode too
CONFOPTS="--prefix=${PGIPATH} --enable-debug"
if [[ "$mode" == d* ]]; then
    CFLAGS="${CFLAGS} -O0 -ggdb -fno-omit-frame-pointer" \
	  "$PGSDIR/configure" $CONFOPTS --enable-tap-tests --enable-cassert
elif [[ "$mode" == r* ]]; then
    CFLAGS="${CFLAGS} -O2" "$PGSDIR/configure" $CONFOPTS
elif [[ "$mode" == p* ]]; then
    CFLAGS="${CFLAGS} -O2 -fno-omit-frame-pointer" "$PGSDIR/configure" $CONFOPTS
else
    echo "Wrong mode"
fi

# run make
echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
makeopts="${silent} ${target}"
if [[ -z "${makeopts// }" ]]; then
    make -j4
else
    make -j4 "${makeopts// }"
fi
echo "Postgres at ${PGSDIR} successfully built"

# run tests, if needed
if [ "$run_tests" = true ]; then
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
    make check
fi

make -j4 -s install
