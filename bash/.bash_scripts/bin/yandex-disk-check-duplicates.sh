#!/bin/bash

set -e

show_help() {
    cat << EOF
    Usage:
      bash ${0##*/} <options>
    
    Options:
      -l: list finds duplicates starting with . dir recursively and shows them.
      -r: removes duplicates listed with list.
      -c: count them 
      -h: display this help and exit
EOF
    exit 0
}

action=$1
if [[ $action == -h* ]];
then
  show_help
fi

duplicates="$(find . -regex ".+ ([0-9]+)\'" 2>/dev/null)"
if [[ $action == -l* ]];
then
  echo "${duplicates}"
elif [[ $action == -c* ]];
then
  echo `echo -n "${duplicates}" | wc -l`
elif [[ $action == -r* ]];
then
  echo "${duplicates}" | xargs -d '\n' rm 
else
  show_help
fi
