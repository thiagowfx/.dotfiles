#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack bash bin fzf ghostty git profile ranger ssh tmux vim zsh"
dotfiles_dir := justfile_directory()
target_dir := env_var("HOME")

# List available commands
@_list:
    just --list

# Run ansible
ansible: _ansible-galaxy ansible-playbook

_ansible-galaxy:
    ansible-galaxy install -r ansible/requirements.yml

# Run ansible playbook
ansible-playbook:
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

    stow -t {{ target_dir }} -d {{ dotfiles_dir }} {{ packages }}

# Check for dangling symlinks
stow-lint:
    chkstow -t {{ target_dir }}

# Remove all symlinks
unstow:
    stow -t {{ target_dir }} -d {{ dotfiles_dir }} --delete {{ packages }}

# Install dotfiles
install: stow ansible

# Update git submodules and pre-commit hooks
update: update-git update-pre-commit

# Update git submodules
update-git:
    git submodule update --force --remote --jobs "$(nproc)"

# Update pre-commit hooks
update-pre-commit:
    pre-commit autoupdate --freeze --jobs "$(nproc)" && pre-commit run --all-files
