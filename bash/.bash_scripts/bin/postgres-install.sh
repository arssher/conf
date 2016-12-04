#!/usr/bin/env bash
set -e

show_help() {
    cat <<EOF
    Usage: bash ${0##*/} [-l | -r]

    Install Postgres built in \$PGDIR. Then create a cluster in 'data' folder
    inside \$PGIDIR, i.e. installation directory, create table `whoami`, apply
    some configs, possibly install extensions and create some test data.

    -h display this help and exit
    -l build and install llvm extension. \$LLVM_EXT_DIR must point to
       the extension directory.
    -r install reversed executor extension. \$RE_EXT_DIR must point to
       the extension directory.

    Don't install both extensions for now.
EOF
    exit 0
}

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh


install_llvm_extension=""
install_re_extension=""
OPTIND=1 # reset opt counter, it is always must be set to 1
# each symbol is option name; if there is colon after, it has value
# the first colon would mean non-silent mode (error reporting)
while getopts "lrh" opt; do # the result will be stored in $opt
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
	\?) # match '?'
	    show_help >&2
	    exit 1
	    ;;
    esac
done

cd $PGDIR
pkill -9 postgres || true
make -j4 install

rm -rf $PGIDIR/data
$PGIDIR/bin/initdb -D $PGIDIR/data

#echo "max_prepared_transactions = 100" >> $PGIDIR/data/postgresql.conf
#echo "wal_level = hot_standby" >> $PGIDIR/data/postgresql.conf
#echo "wal_keep_segments = 128" >> $PGIDIR/data/postgresql.conf
#echo "max_connections = 10" >> $PGIDIR/data/postgresql.conf
#echo "listen_addresses = '*'" >> $PGIDIR/data/postgresql.conf
#echo "shared_buffers = 1GB" >> $PGIDIR/data/postgresql.conf
#echo "fsync = off" >> $PGIDIR/data/postgresql.conf
#echo "autovacuum = off" >> $PGIDIR/data/postgresql.conf

# disable parallelism
echo "max_parallel_workers_per_gather = 0" >> $PGIDIR/data/postgresql.conf
echo "log_min_messages = debug5" >> $PGIDIR/data/postgresql.conf

# install LLVM extension, if needed.
if [ "install_llvm_extension" = true ]; then
  echo "Installing llvm extension..."
  cd $LLVM_EXT_DIR && make -j4 BACKEND_FILE_LIMIT=15 && make install
  echo "shared_preload_libraries = '\$libdir/llvm_pg'" >> $PGIDIR/data/postgresql.conf
else
  echo "LLVM extension will not be installed"
fi

# install reverse_executor extension, if needed.
if [ "$install_re_extension" = true ]; then
  echo "Installing reversed executor extension..."
  cd $RE_EXT_DIR && make -j4 && make install
  echo "shared_preload_libraries = '\$libdir/reverse_executor_test'" >> $PGIDIR/data/postgresql.conf
else
  echo "Reversed executor extension will not be installed"
fi

echo '' > $PGIDIR/data/logfile

echo "host all all 0.0.0.0/0 trust" >> $PGIDIR/data/pg_hba.conf
echo "host replication all 0.0.0.0/0 trust" >> $PGIDIR/data/pg_hba.conf
echo "local replication all trust" >> $PGIDIR/data/pg_hba.conf

#$PGIDIR/bin/pg_ctl -w -D $PGIDIR/data start
$PGIDIR/bin/pg_ctl -w -D $PGIDIR/data -l $PGIDIR/data/logfile start
$PGIDIR/bin/createdb `whoami`

# Since extension is unstable so far, disable it while creating test data
if [ "$install_re_extension" = true ]; then
    $PGIDIR/bin/psql -c "set enable_reverse_exec_hook TO off;" < "${script_dir}/postgres_common/test_query.sql"
else
    $PGIDIR/bin/psql < "${script_dir}/postgres_common/test_query.sql"
fi
