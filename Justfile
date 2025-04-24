#!/usr/bin/env just --justfile
# https://github.com/casey/just

set dotenv-load

# List of default packages to install
packages := "ack bash bin fzf git profile ranger ssh tmux vim zsh"

dotfilesdir := justfile_directory()
targetdir := env_var("HOME")

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
	if ! command -v git &> /dev/null; then
		echo "No git in PATH, install it first" >&2
		exit 1
	fi
	if ! command -v stow &> /dev/null; then
		echo "No stow in PATH, install it first" >&2
		exit 1
	fi
	stow -t {{targetdir}} -d {{dotfilesdir}} {{packages}}

# Check for dangling symlinks
stow-lint:
	chkstow -t {{targetdir}}

# Remove all symlinks
unstow:
	stow -t {{targetdir}} -d {{dotfilesdir}} --delete {{packages}}

# Default recipe
default: stow

