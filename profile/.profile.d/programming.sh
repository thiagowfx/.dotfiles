#!/bin/sh

# golang: https://go.dev/wiki/GOPATH
if command -v go >/dev/null 2>&1; then
    export GOPATH="$HOME/go"
    path_munge "$GOPATH/bin" "/usr/local/go/bin"
fi

# https://stackoverflow.com/questions/46288847/how-to-suppress-pip-upgrade-warning
command -v python3 >/dev/null 2>&1 && export PIP_DISABLE_PIP_VERSION_CHECK=1

# mise: tool versions, env vars and tasks: https://mise.jdx.dev/
# Shims cover non-interactive shells (scripts, editors, IDEs). Interactive zsh
# additionally gets full PATH activation from .zshrc.d/plugins.zsh.
if command -v mise >/dev/null 2>&1; then
	eval "$(mise activate --shims)"
fi

# rust cargo: https://github.com/rust-lang/cargo
if command -v cargo >/dev/null 2>&1; then
	path_munge "$HOME/.cargo/bin"
fi

# nix: https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian
src_files "$HOME/.nix-profile/etc/profile.d/nix.sh"
