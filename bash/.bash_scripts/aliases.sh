# Hack to make aliases work with sudo, see
# http://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo
alias sudo='sudo '
alias phgrep='cat ~/.persistent_history | grep --text -i --color'
alias brc='source ~/.bashrc'
alias e='emacs -nw'
alias pgs='ps -eF --forest | grep postgres'
alias zps='ps -eF --forest | grep -i -E "([p]ostgres|pageserver|safekeeper|storage_broker)"'
alias gst='git status'
alias cargo-zclippy='~/neon/neon/run_clippy.sh'
alias pyf='cd test_runner/; poetry run black . && poetry run flake8 . && poetry run isort . && poetry run mypy .; cd ..'
alias ruf='cargo fmt --all && cargo fix --allow-dirty --allow-staged && ./run_clippy.sh && cargo test'
alias k=kubectl
complete -o default -F __start_kubectl k
