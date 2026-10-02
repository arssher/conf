#!/bin/bash

set -e

for port in 15432 15433 15434; do
    :
    psql -d "dbname=regression user=postgres host=127.0.0.1 port=${port} application_name=mtm_admin" -c "select * from pg_prepared_xacts;"
done
