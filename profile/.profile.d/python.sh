#!/bin/sh

# https://stackoverflow.com/questions/46288847/how-to-suppress-pip-upgrade-warning
hash python3 >/dev/null 2>&1 && export PIP_DISABLE_PIP_VERSION_CHECK=1

# pyenv: https://github.com/pyenv/pyenv
if hash pyenv >/dev/null 2>&1; then
	export PYENV_ROOT="$HOME/.pyenv"
	path_munge "$PYENV_ROOT/bin"
	eval "$(pyenv init -)"
fi
