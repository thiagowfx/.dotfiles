#!/bin/sh

# golang: https://go.dev/wiki/GOPATH
if command -v go >/dev/null 2>&1; then
    export GOPATH="$HOME/go"
    path_munge "$GOPATH/bin" "/usr/local/go/bin"
fi

# npm nvm: node version manager: https://github.com/nvm-sh/nvm
# bash_completion is intentional
if command -v nvm >/dev/null 2>&1; then
    export NVM_DIR="$HOME/.nvm"
    # shellcheck disable=SC1091
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" && src_files "$NVM_DIR/bash_completion"
fi

# https://stackoverflow.com/questions/46288847/how-to-suppress-pip-upgrade-warning
command -v python3 >/dev/null 2>&1 && export PIP_DISABLE_PIP_VERSION_CHECK=1

# python pyenv: https://github.com/pyenv/pyenv
if command -v pyenv >/dev/null 2>&1; then
	export PYENV_ROOT="$HOME/.pyenv"
	path_munge "$PYENV_ROOT/bin"
	# shellcheck disable=SC2016
	eval "$(pyenv init -)"
fi

# ruby rbenv: https://github.com/rbenv/rbenv
if command -v rbenv >/dev/null 2>&1; then
	path_munge "$HOME/.rbenv/bin"
	# shellcheck disable=SC2016
	eval "$(rbenv init -)"
fi

# rust cargo: https://github.com/rust-lang/cargo
if command -v cargo >/dev/null 2>&1; then
	path_munge "$HOME/.cargo/bin"
fi
