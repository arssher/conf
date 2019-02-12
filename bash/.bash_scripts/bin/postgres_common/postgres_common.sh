#!/usr/bin/env bash

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
