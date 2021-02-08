#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-t target] [-m mode] [-r] [-e <ext,ext,...>] [-s]

    Configure and build Postgres, found in \$PGSDIR, with prefix
    (installation path) \$PGIDIR/\$PGINAME, using \$PGBDIR as a build directory.
    If \$PGBDIR is not specified, it is /tmp/\$PGINAME by default.
    If \$PGIDIR is not specified, it is ~/postgres/install by default.
    Then install it.

    pg_workon in .bashrc makes it easier to use, setting these vars.

    -h display this help and exit
    -t target
       make target, for example, 'install'. By default it is 'install'.
    -m <d[ebug] | r[elease] | p[erf]>, 3 choices:
       debug: no optimizations, asserts, etc, default value
       release, turn on optimizations, disable asserts, etc.
       perf: like release, but with frame pointers not omitted to get callstacks
         on old perfs
    -r run regression tests. By default they are not run.
    -e comma separated-list of extensions in contrib/ to 'install'
    -s silent make
    -d dry-run: just print dirs and exit
    -c clang: build with clang and glldb
    -v valgrind: enable valgrind requests (USE_VALGRIND)

    Examples:
    On bf:
    PGSDIR=~/ars/postgrespro PGBDIR=~/ars/postgrespro PGIDIR=~/ars/install PGINAME=pgpro postgres-build-full -m d
    Non-vpath:
    PGSDIR=~/postgres/pgpro PGBDIR=~/postgres/pgpro postgres-build-full.sh -m d
    Silencing make, but without losing stderr:
    PGSDIR=/home/ars/postgres/postgresql PGINAME=vanilla postgres-build-full.sh -s > /dev/null
EOF
    exit 0
}

mode="debug"
run_tests=""
target="install"
silent=""
extensions=""
dry_run="false"
use_clang="false"
valgrind_requests="false"
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "m:ht:se:dcv" opt; do # the result will be stored in $opt
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
	e)
	    extensions=$OPTARG
	    ;;
	d)
	    dry_run="true"
	    ;;
	c)
	    use_clang="true"
	    ;;
	v)
	    valgrind_requests="true"
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

if [[ "${target}" =~ ^install.* ]] && [ -z "${PGINAME}" ]; then
    echo "PGINAME variable with Postgres installation name is not defined, exiting"
    exit 1
fi
script_dir=`dirname "$(readlink -f "$0")"`

if [ -z "${PGSDIR}" ]; then
    echo "PGSDIR variable with path to Postgres sources directory is not defined, exiting"
    exit 1
fi
if [ -z "${PGIDIR+set}" ]; then
    export PGIDIR="${HOME}/postgres/install"
    echo "PGIDIR variable with path to Postgres installations is not defined, setting it to ${PGIDIR}"
fi
if [ -z "${PGBDIR+set}" ]; then
    export PGBDIR="/tmp/${PGINAME}"
    echo "PGBDIR variable with path to Postgres build directory is not defined, setting it to ${PGBDIR}"
fi
export PGIPATH="${PGIDIR}/${PGINAME}"

echo
echo PGSDIR is "$PGSDIR"
echo PGBDIR is "$PGBDIR"
echo PGIPATH is "$PGIPATH"

if [[ "${dry_run}" == "true" ]]; then
    exit 0
fi

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

# musl setup
# export CC=/usr/local/musl/bin/musl-gcc
# export CFLAGS="${CFLAGS} -no-pie"
# CONFOPTS="${CONFOPTS} --without-zlib"

# basic things
export CFLAGS="${CFLAGS} -std=c99"
# export CFLAGS="${CFLAGS} -std=c99 -Wno-unused-function"

if [[ "${use_clang}" == "true" ]]; then
    export CC=clang
    export CFLAGS="${CFLAGS} -glldb"
else
    # ggdb3 makes gdb aware of macros
    export CFLAGS="${CFLAGS} -ggdb3"
    # excellent gold provides faster linking, but not sure how it plays along
    # with clang
    if command -v gold; then
	export CFLAGS="${CFLAGS} -fuse-ld=gold"
    fi
fi

# opts for proper inlining xxx
export CFLAGS="${CFLAGS} --param large-stack-frame=4096 --param large-stack-frame-growth=100000"

# various stuff

# enabled automatically with --enable-cassert
# export CPPFLAGS="${CPPFLAGS} -DCLOBBER_FREED_MEMORY"

# enable valgrind client requests
if [[ "${valgrind_requests}" == "true" ]]; then
    export CPPFLAGS="${CPPFLAGS} -DUSE_VALGRIND"
fi

# export CPPFLAGS="${CPPFLAGS} -DCLOBBER_CACHE_ALWAYS"
# export CPPFLAGS="${CPPFLAGS} -DOPTIMIZER_DEBUG"

# don't forget to set guc(s) if needed:
#   trace_locks = true
#   trace_lwlocks = true
# export CPPFLAGS="${CPPFLAGS} -DLOCK_DEBUG"

export PYTHON=/usr/bin/python3
# CONFOPTS="${CONFOPTS} --with-python"

# since debug symbols don't affect performance, include them in rel mode too
CONFOPTS="${CONFOPTS} --prefix=${PGIPATH} --enable-debug --enable-depend --enable-tap-tests "
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
    CFLAGS="${CFLAGS} -O0 -fno-omit-frame-pointer -Wno-inline" \
	  "$PGSDIR/configure" $CONFOPTS --enable-cassert
elif [[ "$mode" == r* ]]; then
    CFLAGS="${CFLAGS} -O2" "$PGSDIR/configure" $CONFOPTS
elif [[ "$mode" == p* ]]; then
    CFLAGS="${CFLAGS} -O2 -fno-omit-frame-pointer" "$PGSDIR/configure" $CONFOPTS
else
    echo "Wrong mode"
fi

# purge installation directory to avoid stale files (e.g. extensions) left there
rm -rf ${PGIPATH} && mkdir -p ${PGIPATH}

# run make
export COPT='-Werror' # see the 'installation from source' doc
echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
numcores=$(($(cat /proc/cpuinfo | awk '/^processor/{print $3}' | tail -1) + 1))
makeopts="-j ${numcores} ${silent} ${target}"
make $makeopts
echo "Postgres at ${PGSDIR} successfully built"

# build & install extensions, if needed
extensions_arr=(${extensions//,/ }) # convert ',' to spaces and build the array
for ext in "${extensions_arr[@]}"; do
    cd contrib/$ext && make $silent -j $numcores install && cd ../../
    echo "Extension $ext built & installed"
done

# run tests, if needed
if [ "$run_tests" = true ]; then
    echo '-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-'
    make check
fi
