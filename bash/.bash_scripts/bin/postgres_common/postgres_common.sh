#!/usr/bin/env bash

if [ -z "${PGBDIR+set}" ]; then
    export PGBDIR="${HOME}/tmp/tmp/postgresql_build"
    echo "PGBDIR variable with path to Postgres build directory is not defined, setting it to ${PGBDIR}"
fi

if [ -z "${PGIDIR+set}" ]; then
    export PGIDIR="${HOME}/postgres/install"
    echo "PGIDIR variable with path to Postgres installations is not defined, setting it to ${PGIDIR}"
fi

if [ -z "${PGINAME}" ]; then
    echo "PGINAME variable with Postgres installation name is not defined, exiting"
    exit 1
fi

if [ -z "${PGSDIR}" ]; then
    echo "PGSDIR variable with path to Postgres sources directory is not defined, exiting"
    exit 1
fi

export PGIPATH="${PGIDIR}/${PGINAME}"

echo PGSDIR is "$PGSDIR"
echo PGBDIR is "$PGBDIR"
echo PGIPATH is "$PGIPATH"
