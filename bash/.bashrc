# do this before we exit in non-interfactive shells!
if [ -f ~/.global_vars ]; then
      source ~/.global_vars
fi

# ------------------------------------------------------------
# Stuff from default .bashrc

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm|xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi
if [ "$color_prompt" = yes ]; then
    if [[ ${EUID} == 0 ]] ; then
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\h\[\033[01;34m\] \W \$\[\033[00m\] '
    else
        PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\w \$\[\033[00m\] '
    fi
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h \w \$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
# (Actually I do need)
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# My stuff
# ------------------------------------------------------------
# There are a lot of bullshit above setting PS; let's just have name, host and
# pwd:


if [ -x "$(command -v yandex-disk)" ]; then
    export YANDEXDISK_DIR=`cat ~/.config/yandex-disk/config.cfg | grep "dir=" | sed 's/dir=\"\(.*\)\"/\1/' | grep -E -v '^#.*'`
    export CONFPATH="${YANDEXDISK_DIR}/configs"
fi
export CONFPATH=~/Dropbox/configs

PATH=$PATH:~/.bash_scripts/bin # TODO: perhaps check if it is already added?
export LC_ALL="en_US.UTF-8"


# Read file with machine-dependant global env variables
# include .bashrc if it exists
if [ -f ~/.global_vars ]; then
      source ~/.global_vars
      # echo "global vars loaded"
fi

source ~/.bash_scripts/aliases.sh

# Eternal bash history. It is not reread after every command, so every terminal
# have it's own history.  To search all commands, including typed in other
# terminals, see Bendersky's addition below.
# Sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
# I would unset it, meaning 'eternal', but then gdb will not write history at
# all, see
# http://unix.stackexchange.com/questions/162820/gdb-up-arrow-doesnt-work
# so just setting it to a big value
# (and this is true even though I have set history size unlimited in ~/.gdbinit)
export HISTSIZE=10000000
# note that timestamps in HISTFILE are encoded in bash's own format, and
# human-readable timestamps are shown only with 'history' builtin.
export HISTTIMEFORMAT="[%F %T] "
# erase duplicates
export HISTCONTROL=ignoreboth:erasedups
# Change the file location because certain bash sessions truncate .bash_history
# file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
# XXX people often add the following
# shopt -s histappend
# which means 'when writing to history file, append, not replace',
# https://www.gnu.org/software/bash/manual/html_node/Bash-History-Facilities.html
# They try to avoid losing history from parallel shells this way: with default
# settings, last closed wins and saves all history existing before it was started
# + its own history, the rest is essentially dropped. However, with `history -a`
# after each command this is not needed. Moreover, I suspect with histappend
# history will be duplicated on each save, but need to check that

# also log all commands to ~/.persistent_history and search it with phgrep.
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
PROMPT_COMMAND="log_bash_persistent_history; $PROMPT_COMMAND"

# use emacs as an editor everywhere
export VISUAL=emacs
export EDITOR="$VISUAL"

# x4: Postgres uses 4-spaces tab, set up 'less' for displaying it.
# R option preserves ANSI color escape sequences, i.e. lets to colorize the
# terminal.
export LESS=-x4R

# Map right alt to behave as ctrl. Doesn't work in X
if [ -x "$(command -v setxkbmap)" ]; then
    setxkbmap -option ctrl:ralt_rctrl
fi;
# TODO: check the following:
# logger "setting altgr -> ctrl"
# DISPLAY=:0 XDG_RUNTIME_DIR=/run/user/$(id -u) setxkbmap -option ctrl:ralt_rctrl

# always dump core
ulimit -c unlimited

# beatiful git log as `git log`
git config --global alias.lg "log --color --graph --abbrev-commit
           --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset'"


# k8s autocompletion
# https://kubernetes.io/docs/reference/kubectl/cheatsheet/
source <(kubectl completion bash)

# Use virtualenvwrapper, if it is available
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
      source /usr/local/bin/virtualenvwrapper.sh
fi

# usage: neks dev-eu-west
# or better just create aliases
function neks {
    kubectx $(kubectx | grep $1)
}

# quickly switch pg currently in use by outraging PATH
# $1 is installation name
# If $2 is given, it is the source name (under ~/postgres/);
# otherwise source name is assumed the same as inst name
function pg_workon {
    export PATH="${HOME}/postgres/install/${1}/bin:${PATH}"
    if [ -z "${2}" ]; then
	export PGSDIR="${HOME}/postgres/${1}"
    else
	export PGSDIR="${HOME}/postgres/${2}"
    fi
    export PGINAME="${1}"
}
function pg_re {
    make -C "/tmp/${PGINAME}" -j4 install
}
function pg_norsu_workon {
    export PATH="${HOME}/tmp/tmp/norsu/${1}/bin/:${PATH}"
}
function whichpg {
    echo "PGSDIR=${PGSDIR}"
    echo "PGINAME=${PGINAME}"
    echo "PGBDIR=${PGBIDR}"
    echo "PGIDIR=${PGIDIR}"
    echo -n "which pg_ctl "
    which pg_ctl
}

function mtm_psql {
    export PGHOST=127.0.0.1
    export PGDATABASE=regression
}

# remove core & tmp files
function rmc {
    rm -rf /tmp/tmp*
    rm -rf /tmp/core*
    # testgres
    rm -rf /tmp/tgsn_*
    rm -rf /tmp/tgsb_*
    # stolon
    rm -rf /tmp/stolon*
}

function stophp {
    pkill stolon-keeper || true
    pkill stolon-sentinel || true
    pkill -9 postgres || true
    pkill etcd || true
}

# Disable XON/XOFF flow control so we can use Ctrl-s for searching forward. See
# https://wiki.archlinux.org/index.php/readline
# https://unix.stackexchange.com/questions/12107/how-to-unfreeze-after-accidentally-pressing-ctrl-s-in-a-terminal
stty -ixon -ixoff
source "$HOME/.cargo/env"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

source "$HOME/.bash_scripts/utils.sh"
if [ -f $HOME/.cargo/env ]; then
    . "$HOME/.cargo/env"
fi
