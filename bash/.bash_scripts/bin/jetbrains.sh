#!/bin/bash

set -e

show_help() {
    cat << EOF
    Usage: bash ${0##*/} [-n NAME] [-u URL]
    Download tar.gz NAME=<idea|pycharm|clion> jetbrains product from URL to JETBRAINS_DIR, extract it and add {{PRODUCTNAME}}BIN variable with path to executable to .global_vars. It will be used to create a shorcut.

        -h          display this help and exit
        -n NAME     <idea|pycharm|clion>
        -u URL      url to download from, https://download.jetbrains.com/python/pycharm-professional-5.0.4.tar.gz, for example
EOF
    exit 0
}

update_env_var() {
    name=$1
    foldername=$2
    env_var_name="${name^^}_BIN"
    bin_path="${JETBRAINS_DIR}/${foldername}/bin/${name}.sh"
    echo "name=${name}, foldername=${foldername}, env_var_name=${env_var_name}, bin_path=${bin_path}"
    # delete old entry if exists
    sed "/^export ${env_var_name}=/ d" < ~/.global_vars > ~/.global_vars_tmp
    # add entry
    echo "export ${env_var_name}=${bin_path}" >> ~/.global_vars_tmp
    cp ~/.global_vars_tmp ~/.global_vars
    rm ~/.global_vars_tmp
    source ~/.bashrc
}

# check if JETBRAINS_DIR variable is present
if [ -z ${JETBRAINS_DIR+x} ]; then
  echo "JETBRAINS_DIR variable is unset, exiting"
  exit 1
fi

name=""
url=""
OPTIND=1
while getopts ":n:u:" opt; do
    case "$opt" in
        h)
            show_help
            exit 0
            ;;
        n)  name=$OPTARG
            ;;
        u)  url=$OPTARG
            ;;
        '?')
            show_help >&2
            exit 1
            ;;
    esac
done
echo "Args: name=${name} and url=${url}. Jetbrains dir is ${JETBRAINS_DIR}"

targzfilename=`basename "${url}"`
wget -P "${JETBRAINS_DIR}" "${url}"
cd "${JETBRAINS_DIR}"
foldername=`tar tzf ${targzfilename} | sed -e 's@/.*@@' | uniq`
tar -xzf "${targzfilename}"
rm "${targzfilename}"
update_env_var "${name}" "${foldername}"

#update shortcuts
cd "${CONFPATH}/mint_shortcuts"
bash "${CONFPATH}/mint_shortcuts/ydisk_to_machine.sh"
