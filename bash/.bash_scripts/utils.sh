function ph {
    pattern="${1}"
    offset="${2}"
    if [ -z "$offset" ]
    then
	offset=1
    fi

    cat ~/.persistent_history | grep --text -v -E '(ph )|(phgrep )' | grep --text -i --color "${pattern}"
    cat ~/.persistent_history |  grep --text -v -E '(ph )|(phgrep )' | grep --text -i "${pattern}" | tail -n ${offset} | head -n 1 | cut -d '|' -f 2
    entry_to_copy=$(cat ~/.persistent_history |  grep --text -v -E '(ph )|(phgrep )' | grep --text -i "${pattern}" | tail -n ${offset} | head -n 1)
    # remove stuff before " | "
    searchstring=" | "
    cmd_to_copy=${entry_to_copy#*$searchstring}
    # save the cmd to the clipboard
    echo "copied to buffer: ${cmd_to_copy}"
    echo -n ${cmd_to_copy} | xclip  # to mouse
    echo -n ${cmd_to_copy} | xclip -sel clip  # to usual
}

function neon_ep {
    ep_name="${1}"
    echo $(kubectl get pods | grep ${ep_name} | awk '{print $1}')
}

function nkill {
    pkill pageserver; pkill postgres; pkill safekeeper; pkill storage_broker; pkill pytest;
}

function git_ub {
    git checkout main && git branch -D $1; git fetch && git switch -c $1 origin/$1
}
