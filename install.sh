#!/bin/bash
#
# Installs the dotfiles environment locally to ~/.dotfiles.

set -xeuo pipefail

# Full path to this script directory.
readonly SCRIPTDIR=$(dirname "$(readlink -f "$0")")

# Checks out latest git submodules, in case the repository was not recursively cloned.
(cd "$SCRIPTDIR" && git submodule update --init)

# Runs stow.
(cd "$SCRIPTDIR" && stow -t "$HOME" -d "$SCRIPTDIR" --verbose=2 --restow main corp)

# Installs ranger configs.
ranger --copy-config=all

# Installs tmux plug-ins.
"$HOME/.tmux/plugins/tpm/bin/install_plugins"

# Installs vim plug-ins.
vim +PlugClean! +PlugInstall +qall
