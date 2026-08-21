#!/bin/sh

# mise: tool versions, env vars and tasks: https://mise.jdx.dev/
# Shims cover non-interactive shells (scripts, editors, IDEs). Interactive zsh
# additionally gets full PATH activation from .zshrc.d/plugins.zsh.
if command -v mise >/dev/null 2>&1; then
	eval "$(mise activate --shims)"
fi

# nix: https://ariya.io/2020/05/nix-package-manager-on-ubuntu-or-debian
src_files "$HOME/.nix-profile/etc/profile.d/nix.sh"
