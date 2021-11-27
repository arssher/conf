# Hack to make aliases work with sudo, see
# http://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo
alias sudo='sudo '
alias phgrep='cat ~/.persistent_history | grep --text -i --color'
alias brc='source ~/.bashrc'
alias e='emacs -nw'
alias ystart='yandex-disk start'
alias ystop='yandex-disk stop'
alias pgs='ps -eF --forest | grep postgres'
alias zps='ps -eF --forest | grep -i -E "([p]ostgres|pageserver|safekeeper)"'
alias cargo-zclippy='~/zenith/zenith/run_clippy.sh'
