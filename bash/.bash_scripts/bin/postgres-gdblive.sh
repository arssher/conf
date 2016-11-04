#!/bin/sh

# Usage: gdblive [ arguments to grep output of ps ]

cd $HOME

# tee /dev/tty is for user to see the set of procs considered
if [ $# -eq 0 ]
then
        PROCS=`ps auxww | \
        grep postgres: | \
        grep -v -e 'grep postgres:' -e 'postgres: stats' -e 'postgres: writer' -e 'postgres: wal writer' -e 'postgres: checkpointer' -e 'postgres: archiver' -e 'postgres: logger' -e 'postgres: autovacuum' | \
        tee /dev/tty | \
        awk '{print $2}'`
else
        PROCS=`ps auxww | \
        grep postgres: | \
        grep -v -e 'grep postgres:' -e 'postgres: stats' -e 'postgres: writer' -e 'postgres: wal writer' -e 'postgres: checkpointer' -e 'postgres: archiver' -e 'postgres: logger' -e 'postgres: autovacuum' | \
        grep $@ | \
        tee /dev/tty | \
        awk '{print $2}'`
fi

if [ `echo "$PROCS" | wc -w` -eq 1 ]
then
    exec gdb $PGIDIR/bin/postgres -silent "$PROCS"
else
    exec gdb $PGIDIR/bin/postgres -silent
fi
