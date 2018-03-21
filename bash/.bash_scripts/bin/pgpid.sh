#!/bin/sh

# print PID of Postgres backend process

ps aux | grep '[p]ostgres: ars' | tail -n 1 | awk '{print $2}'
