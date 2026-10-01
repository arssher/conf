#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-l | -r | -c]

    Install Postgres built in \$PGDIR. Then create a cluster in 'data' folder
    inside \$PGIDIR, i.e. installation directory, create table `whoami`, apply
    some configs, possibly install extensions and create some test data.

    -h display this help and exit
    -l build and install llvm extension. \$LLVM_EXT_DIR must point to
       the extension directory.
    -r install reversed executor extension. \$RE_EXT_DIR must point to
       the extension directory.
    -c Create a cluster in \$PGDATADIR directory, create table `whoami`, apply
       some configs, start cluster. Use this if you need simple test cluster and
       don't have one

    Don't install both extensions for now.
EOF
    exit 0
}

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

install_llvm_extension=""
install_re_extension=""
create_cluster=""
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "lrch" opt; do # the result will be stored in $opt
    case $opt in
	h) # bracket is a part of case syntax, you know
	    show_help
	    exit 0
	    ;;
	l)
	    install_llvm_extension=true
	    ;;
	r)
	    install_re_extension=true
	    ;;
	c)
	    create_cluster=true
	    ;;
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

cd $PGDIR
pkill -9 postgres || true
make -j4 install

# install LLVM extension, if needed.
if [ "$install_llvm_extension" = true ]; then
  echo "Installing llvm extension..."
  cd $LLVM_EXT_DIR && make mostlyclean && make -j4 BACKEND_FILE_LIMIT=15 && make install
else
  echo "LLVM extension will not be installed"
fi

# install reverse_executor extension, if needed.
if [ "$install_re_extension" = true ]; then
  echo "Installing reversed executor extension..."
  cd $RE_EXT_DIR && make clean && make -j4 && make install
else
  echo "Reversed executor extension will not be installed"
fi

if ! [ "$create_cluster" = true ]; then
    # no need to create cluster, exiting
    exit 0
fi

echo "Creating test cluster..."
if [ -z "$PGDATADIR" ]; then
    echo "You asked to create a cluster, but PGDATADIR is empty, exiting"
    exit 1
fi
rm -rf "$PGDATADIR"
$PGIDIR/bin/initdb -D "$PGDATADIR"

#echo "max_prepared_transactions = 100" >> "$PGDTADIR"/postgresql.conf
#echo "wal_level = hot_standby" >> "$PGDTADIR"/postgresql.conf
#echo "wal_keep_segments = 128" >> "$PGDTADIR"/postgresql.conf
#echo "max_connections = 10" >> "$PGDTADIR"/postgresql.conf
#echo "listen_addresses = '*'" >> "$PGDTADIR"/postgresql.conf
#echo "shared_buffers = 1GB" >> "$PGDTADIR"/postgresql.conf
#echo "fsync = off" >> "$PGDTADIR"/postgresql.conf
#echo "autovacuum = off" >> "$PGDTADIR"/postgresql.conf

# disable parallelism
echo "max_parallel_workers_per_gather = 0" >> "$PGDTADIR"/postgresql.conf
echo "log_min_messages = debug5" >> "$PGDTADIR"/postgresql.conf

echo '' > "$PGDTADIR"/logfile

echo "host all all 0.0.0.0/0 trust" >> "$PGDTADIR"/pg_hba.conf
echo "host replication all 0.0.0.0/0 trust" >> "$PGDTADIR"/pg_hba.conf
echo "local replication all trust" >> "$PGDTADIR"/pg_hba.conf

# add LLVM ext to config, if needed.
if [ "$install_llvm_extension" = true ]; then
  echo "shared_preload_libraries = '\$libdir/llvm_pg'" >> "$PGDTADIR"/postgresql.conf
fi

# add reverse ext to config, if needed.
if [ "$install_re_extension" = true ]; then
  echo "shared_preload_libraries = '\$libdir/reverse_executor_test'" >> "$PGDATADIR"/postgresql.conf
fi

#$PGIDIR/bin/pg_ctl -w -D "$PGDTADIR" start
$PGIDIR/bin/pg_ctl -w -D "$PGDTADIR" -l "$PGDTADIR"/logfile start
$PGIDIR/bin/createdb `whoami`

$PGIDIR/bin/psql < "${script_dir}/postgres_common/test_query.sql"
