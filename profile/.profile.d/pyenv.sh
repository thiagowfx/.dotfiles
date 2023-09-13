# pyenv: https://github.com/pyenv/pyenv
export PYENV_ROOT="$HOME/.pyenv"
hash pyenv >/dev/null 2>&1 || path_munge "$PYENV_ROOT/bin"
eval "$(pyenv init -)"
