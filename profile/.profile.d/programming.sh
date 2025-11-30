#!/bin/sh

# golang: https://go.dev/wiki/GOPATH
if hash go >/dev/null 2>&1; then
    export GOPATH="$HOME/go"
    path_munge "$GOPATH/bin" "/usr/local/go/bin"
fi

# Rancher Desktop
path_munge "$HOME/.rd/bin"

# npm nvm: node version manager: https://github.com/nvm-sh/nvm
# bash_completion is intentional
if hash nvm >/dev/null 2>&1; then
    export NVM_DIR="$HOME/.nvm"
    # shellcheck disable=SC1091
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" && src_files "$NVM_DIR/bash_completion"
fi

# https://stackoverflow.com/questions/46288847/how-to-suppress-pip-upgrade-warning
hash python3 >/dev/null 2>&1 && export PIP_DISABLE_PIP_VERSION_CHECK=1

# python pyenv: https://github.com/pyenv/pyenv
if hash pyenv >/dev/null 2>&1; then
	export PYENV_ROOT="$HOME/.pyenv"
	path_munge "$PYENV_ROOT/bin"
	if [ -n "${ZSH_VERSION:-}" ] && command -v zsh-defer >/dev/null 2>&1; then
		# shellcheck disable=SC2016
		zsh-defer -c 'eval "$(pyenv init -)"'
	else
		# shellcheck disable=SC2016
		eval "$(pyenv init -)"
	fi
fi

# ruby rbenv: https://github.com/rbenv/rbenv
if hash rbenv >/dev/null 2>&1; then
	path_munge "$HOME/.rbenv/bin"
	if [ -n "${ZSH_VERSION:-}" ] && command -v zsh-defer >/dev/null 2>&1; then
		# shellcheck disable=SC2016
		zsh-defer -c 'eval "$(rbenv init -)"'
	else
		# shellcheck disable=SC2016
		eval "$(rbenv init -)"
	fi
fi

# rust cargo: https://github.com/rust-lang/cargo
path_munge "$HOME/.cargo/bin"
