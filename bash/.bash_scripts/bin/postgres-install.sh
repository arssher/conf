#!/usr/bin/env bash

script_dir=`dirname "$(readlink -f "$0")"`
source "$script_dir"/postgres_common/postgres_common.sh

cd $PGDIR
pkill -9 postgres
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

#echo "log_min_messages = debug5" >> $PGIDIR/data/postgresql.conf
#echo "shared_preload_libraries = '$libdir/llvm_pg'" >> $PGIDIR/data/postgresql.conf

							      
echo '' > $PGIDIR/data/logfile				      
							      
echo "host all all 0.0.0.0/0 trust" >> $PGIDIR/data/pg_hba.conf    
echo "host replication all 0.0.0.0/0 trust" >> $PGIDIR/data/pg_hba.conf
echo "local replication all trust" >> $PGIDIR/data/pg_hba.conf     

#$PGIDIR/bin/pg_ctl -w -D $PGIDIR/data start
$PGIDIR/bin/pg_ctl -w -D $PGIDIR/data -l $PGIDIR/data/logfile start
$PGIDIR/bin/createdb `whoami`
$PGIDIR/bin/psql -c "create table test(k int primary key, v text);"

