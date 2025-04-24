#!/usr/bin/env just --justfile
# https://github.com/casey/just

set dotenv-load

# List of default packages to install
packages := "ack bash bin fzf git profile ranger ssh tmux vim zsh"

dotfiles_dir := justfile_directory()
target_dir := env_var("HOME")

# Default recipe
default: stow

# List available commands
@list:
    just --list

# Update git submodules
update:
	git submodule update --remote
	pre-commit autoupdate

# Run ansible playbook
ansible:
	ansible-galaxy install -r ansible/requirements.yml
	ANSIBLE_CONFIG=ansible/ansible.cfg ansible-playbook ansible/bootstrap.yml -i ansible/inventory.ini --tags untagged

# Stow all packages
stow:
	#!/usr/bin/env bash
	set -euo pipefail

	for cmd in git stow; do
	    if ! command -v $cmd &> /dev/null; then
	        echo "No $cmd in PATH, install it first" >&2
	        exit 1
	    fi
	done

	stow -t {{target_dir}} -d {{dotfiles_dir}} {{packages}}

# Check for dangling symlinks
stow-lint:
	chkstow -t {{target_dir}}

# Remove all symlinks
unstow:
	stow -t {{target_dir}} -d {{dotfiles_dir}} --delete {{packages}}

