#!/bin/bash -u
# Script to install the dotfiles environment locally.

# Full path to the dotfiles directory.
readonly DOTFILESDIR=$(dirname "$(readlink -f "$0")")

# Check out latest git submodules, in case the repository was not recursively cloned.
git -C "$DOTFILESDIR" submodule update --init

# Run stow.
for package in main corp; do
	stow -t "$HOME" -d "$DOTFILESDIR" -R "$package"
done

# Install tmux plug-ins.
"$HOME/.tmux/plugins/tpm/bin/install_plugins"

# Install vim plug-ins.
vim +PlugClean! +PlugInstall +qall