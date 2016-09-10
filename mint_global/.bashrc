IMDGTEST_PROPS="/home/ars/sber-pilot/ansible/templates/IMDGTEST_PROPS.j2"
YANDEXDISK_DIR=`cat ~/.config/yandex-disk/config.cfg | grep "dir=" | sed 's/dir=\"\(.*\)\"/\1/'`
export LC_ALL="en_US.UTF-8"

# Eternal bash history. It is not reread after every command, so every terminal have it's own history.
# To search all commands, including typed in other terminals, see Bendersky's addition below.
# ---------------------
# Sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# erase duplicates
export HISTCONTROL=ignoreboth:erasedups
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"


# Log all commands to ~/.persistent_history and search it with phgrep. 
log_bash_persistent_history()
{
  [[
    $(history 1) =~ ^\ *[0-9]+\ +([^\ ]+\ [^\ ]+)\ +(.*)$
  ]]
  local date_part="${BASH_REMATCH[1]}"
  local command_part="${BASH_REMATCH[2]}"
  if [ "$command_part" != "$PERSISTENT_HISTORY_LAST" ]
  then
    echo $date_part "|" "$command_part" >> ~/.persistent_history
    export PERSISTENT_HISTORY_LAST="$command_part"
  fi
}
PROMPT_COMMAND="log_bash_persistent_history; $PROMT_COMMAND"
alias phgrep='cat ~/.persistent_history|grep --color'
