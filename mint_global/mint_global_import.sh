#!/bin/bash

set -e

curr_dir=`dirname "$(readlink -f "$0")"`
configs_dir=$(dirname ${curr_dir})

bash "${configs_dir}/emacs/restore_conf.sh"
bash "${configs_dir}/mint_shortcuts/import.sh"
