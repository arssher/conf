#!/bin/sh

# print PID of Postgres backend process

ps auxww | \
    grep postgres: | \
    grep -v -e 'grep postgres:' -e 'postgres: stats' -e 'postgres: writer' -e \
         'postgres: wal writer' -e 'postgres: checkpointer' \
         -e 'postgres: archiver' -e 'postgres: logger' -e 'postgres: autovacuum' \
         -e 'grep' | \
    awk '{print $2}'
