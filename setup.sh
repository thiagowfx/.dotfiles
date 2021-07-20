#!/bin/bash
# Script to manage the dotfiles environment.

# Full path to the dotfiles directory.
DOTFILESDIR=$(dirname "$(readlink -f "$0")")

report() {
	echo "--> Setting up $1..."
}

usage() {
	echo "Usage: $0 [--install | install | --update | update]" 1>&2
	exit 1
}

install() {
	# Check out git submodules, ensuring the repository is recursively cloned.
	report "git submodules"
	git -C "$DOTFILESDIR" submodule update --init

	# Run stow, symlinking dotfiles.
	report "stow"
	stow -t "$HOME" -d "$DOTFILESDIR" main

	# Install all tmux plug-ins.
	report "tmux plug-ins"
	"$HOME/.tmux/plugins/tpm/bin/install_plugins"

	# Install all vim plug-ins.
	report "vim plug-ins"
	vim +PlugClean! +PlugInstall +qall
}

update() {
	# Update all git submodules to their latest tips.
	report "git submodules update"
	git -C "$DOTFILESDIR" submodule update --recursive --remote

	# Re-run stow, symlinking dotfiles.
	report "stow update"
	stow -t "$HOME" -d "$DOTFILESDIR" --restow main

	# Update all tmux plug-ins.
	report "tmux plug-in updates"
	"$HOME/.tmux/plugins/tpm/bin/update_plugins" all

	# Update all vim plug-ins.
	#   PlugUpgrade: Upgrade vim-plug itself
	#   PlugUpdate: Install or update plugins
	report "vim plug-in updates"
	vim +PlugClean! +PlugUpgrade +PlugUpdate +qall
}

case "$1" in
	--install | install)
		install
		;;
	--update | update)
		update
		;;
	*)
		usage
		;;
esac
