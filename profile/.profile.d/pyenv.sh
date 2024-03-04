# pyenv: https://github.com/pyenv/pyenv
if hash pyenv >/dev/null 2>&1; then
	export PYENV_ROOT="$HOME/.pyenv"
	path_munge "$PYENV_ROOT/bin"
	eval "$(pyenv init -)"
fi
