#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack bash fzf ghostty git profile ranger ssh tmux vim zsh"
packages_no_folding := "espanso swiftbar"
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
    stow -t {{ target_dir }} -d {{ dotfiles_dir }} --no-folding {{ packages_no_folding }}

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

# Update local JSON schemas
update-schemas:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "=== Updating JSON Schemas ==="
    echo ""

    # Create schemas directory if it doesn't exist
    mkdir -p schemas

    # Define schema mappings (filename -> URL)
    declare -A schemas=(
        ["espanso-match.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/match.schema.json"
        ["espanso-config.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/config.schema.json"
    )

    failed=0
    succeeded=0

    # Download each schema
    for filename in "${!schemas[@]}"; do
        url="${schemas[$filename]}"
        output_path="schemas/$filename"

        echo -n "Downloading $filename... "
        if curl -sL -o "$output_path" "$url"; then
            echo "✓"
            succeeded=$((succeeded + 1))
        else
            echo "✗"
            failed=$((failed + 1))
        fi
    done

    echo ""
    if [[ $failed -eq 0 ]]; then
        echo "✓ All $succeeded schemas updated successfully"
        exit 0
    else
        echo "✗ Failed to update $failed schema(s), $succeeded succeeded"
        exit 1
    fi
