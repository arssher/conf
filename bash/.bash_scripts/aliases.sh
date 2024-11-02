# Hack to make aliases work with sudo, see
# http://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo
alias sudo='sudo '
alias brc='source ~/.bashrc'
alias e='emacs -nw'
alias pgs='ps -eF --forest | grep postgres'
alias zps='ps -eF --forest | grep -i -E "([p]ostgres|pageserver|safekeeper|storage_broker)"'
alias zk='pkill pageserver; pkill postgres; pkill safekeeper; pkill pytest; pkill compute_ctl; pkill -f -sigkill storage_controller'
alias gst='git status'
alias cargo-zclippy='~/neon/neon/run_clippy.sh'
alias pyf='poetry run ruff format . && poetry run ruff check --fix . && poetry run mypy .'
alias ruf='cargo fmt --all && cargo fix --allow-dirty --allow-staged && ./run_clippy.sh && cargo test'
alias k=kubectl
complete -o default -F __start_kubectl k
