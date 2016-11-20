export YANDEXDISK_DIR=`cat ~/.config/yandex-disk/config.cfg | grep "dir=" | sed 's/dir=\"\(.*\)\"/\1/'`
PATH=$PATH:~/.bash_scripts/bin # TODO: perhaps check if it is already added?
export LC_ALL="en_US.UTF-8"


# Read file with machine-dependant global env variables
# include .bashrc if it exists
if [ -f ~/.global_vars ]; then
      source ~/.global_vars
      # echo "global vars loaded"
fi

source ~/.bash_scripts/aliases.sh

# Eternal bash history. It is not reread after every command, so every terminal have it's own history.
# To search all commands, including typed in other terminals, see Bendersky's addition below.
# ---------------------
# Sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
# I would unset it, meaning 'eternal', but then gdb will not write history at
# all, see
# http://unix.stackexchange.com/questions/162820/gdb-up-arrow-doesnt-work
# so just setting it to a big value
# (and this is true even though I have set history size -1 in ~/.gdbinit)
export HISTSIZE=10000000
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

