#!/bin/sh

# golang: https://go.dev/wiki/GOPATH
if hash go >/dev/null 2>&1; then
    export GOPATH="$HOME/go"
    path_munge "$GOPATH/bin" "/usr/local/go/bin"
fi

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
	eval "$(pyenv init -)"
fi

# ruby rbenv: https://github.com/rbenv/rbenv
if hash rbenv >/dev/null 2>&1; then
	path_munge "$HOME/.rbenv/bin"
	eval "$(rbenv init -)"
fi

# rust cargo: https://github.com/rust-lang/cargo
path_munge "$HOME/.cargo/bin"
