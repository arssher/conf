#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-t target] [-f] [-r]

    Configure and build Postgres, found in \$PGSDIR, with prefix
    (installation path) \$PGIDIR/\$PGINAME, using \$PGBDIR as a build directory.
    If $PGBDIR is not specified, it is $HOME/tmp/tmp/$PGINAME by defaul.
    Then install it.

    -h display this help and exit
    -t target
       make target, for example, 'install'. By default it is 'install-world'.
    -m <d[ebug] | r[elease] | p[erf]>, 3 choices:
       debug: no optimizations, asserts, etc, default value
       release, turn on optimizations, disable asserts, etc.
       perf: like release, but with frame pointers not omitted to get callstacks
         on old perfs
    -r run regression tests. By default they are not run.
    -s silent make

    Examples:
    Silencing make, but without losing stderr:
    PGSDIR=/home/ars/postgres/postgresql PGINAME=vanilla postgres-build-full.sh -s > /dev/null
EOF
    exit 0
}

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

mode="debug"
run_tests=""
target="install-world"
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

# make sure src and build dirs are absolutely clean, nasty bugs may arise
# if not doing this
# It is very important to run this target, not distclean or clean!
# distclean leaves some files, see
# https://www.postgresql.org/message-id/flat/20050620231820.GB8840%40mits.lv#20050620231820.GB8840@mits.lv

# || true because it will fail if it is a clean postgres
cd "${PGSDIR}" && make maintainer-clean || true
if [ -d "$PGBDIR" ]; then
    # || true because it will fail if it is a clean postgres (configure was never
    # run)
    cd $PGBDIR && make maintainer-clean || true
fi

if ! [[ ${PGBDIR} -ef ${PGSDIR} ]]; then
    echo "VPATH build"
    rm -rf $PGBDIR && mkdir -p $PGBDIR && cd $PGBDIR
else
    echo "usual build"
fi

# run configure
# opts for proper inlining
CFLAGS="${CFLAGS} -std=c99 -Wno-unused-function"
CFLAGS="${CFLAGS} --param large-stack-frame=4096 --param large-stack-frame-growth=100000"
# since debug symbols don't affect perfomance, include them in rel mode too
CONFOPTS="--prefix=${PGIPATH} --enable-debug"
if grep -q "PGPRO_VERSION" "${PGSDIR}/src/include/pg_config.h.in"; then
    :
    # seems like we are building pgpro
    CONFOPTS="${CONFOPTS} --enable-nls \
	--with-openssl --with-perl --with-tcl --with-python \
	--with-gssapi --with-includes=/usr/include/gssglue \
	--with-libxml --with-libxslt --with-ldap \
	--with-icu --with-zstd"
fi
if [[ "$mode" == d* ]]; then
    # ggdb3 makes gdb aware of macros
    CFLAGS="${CFLAGS} -O0 -ggdb3 -fno-omit-frame-pointer -Wno-inline" \
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
numcores=`cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1`
makeopts="-j ${numcores} ${silent} ${target}"
make -j $numcores $makeopts
echo "Postgres at ${PGSDIR} successfully built"

# run tests, if needed
if [ "$run_tests" = true ]; then
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
    make check
fi
