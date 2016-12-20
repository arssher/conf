#!/usr/bin/env bash

if [ -z "${PGDIR+set}" ]; then
    echo "PGDIR variable with path to Postgres build directory is not defined, exiting"
    exit 1
fi

if [ -z "${PGIDIR+set}" ]; then
    echo "PGIDIR variable with path to Postgres installation directory is not defined, exiting"
    exit 1
fi

if [ -z "${PGSDIR}" ]; then
    PGSMakefile=`readlink -f $PGDIR/Makefile` || true
    if [ -n "${PGSMakefile}" ]; then
	export PGSDIR=$(dirname $PGSMakefile)
    fi
fi

if [ -z "${PGSDIR}" ]; then
    echo "PGSDIR variable with path to Postgres sources directory is not defined and could not be deduced"
    exit 1
fi

echo PGIDIR is $PGIDIR
echo PGDIR is $PGDIR
echo PGSDIR is $PGSDIR
