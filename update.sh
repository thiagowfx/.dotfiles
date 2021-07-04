#!/bin/bash -u
# Script to install the dotfiles environment locally.

# Full path to the dotfiles directory.
DOTFILESDIR=$(dirname "$(readlink -f "$0")")

# Update all git submodules to their latest tips.
git -C "$DOTFILESDIR" submodule update --recursive --remote

# Update all tmux plug-ins.
"$HOME/.tmux/plugins/tpm/bin/update_plugins" all

# Update all vim plug-ins.
vim +PlugUpgrade +PlugUpdate +qall
