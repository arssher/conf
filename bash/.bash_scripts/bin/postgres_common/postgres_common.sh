#!/usr/bin/env bash

if [ -z ${PGIDIR+x} ]; then
    echo "PGDIR variable with path to Postgres sources directory is not defined, exiting"
    exit 1
fi

if [ -z ${PGIDIR+x} ]; then
    echo "PGIDIR variable with path to Postgres installation directory is not defined, exiting"
    exit 1
fi

if [ -z ${PGBDIR+x} ]; then
    echo "PGBDIR variable with path to Postgres build directory is not defined, exiting"
    exit 1
fi
