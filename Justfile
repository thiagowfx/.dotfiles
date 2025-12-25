#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack bash ghostty git profile ranger ssh tmux vim zsh"
packages_no_folding := "espanso swiftbar"
[private]
_dotfiles_dir := justfile_directory()
target_dir := env_var("HOME")

# List available commands
@_list:
    just --list

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

    stow -t {{ target_dir }} -d {{ _dotfiles_dir }} {{ packages }}
    stow -t {{ target_dir }} -d {{ _dotfiles_dir }} --no-folding {{ packages_no_folding }}

# Check for dangling symlinks
stow-lint:
    chkstow -t {{ target_dir }}

# Remove all symlinks
unstow:
    stow -t {{ target_dir }} -d {{ _dotfiles_dir }} --delete {{ packages }}

# Install Xcode Command Line Tools
xcode-command-line-tools:
    #!/usr/bin/env bash
    set -euo pipefail

    if ! command -v xcode-select &> /dev/null; then
        echo "Installing Xcode Command Line Tools..."
        xcode-select --install
    else
        echo "Xcode Command Line Tools already installed"
    fi

# Install dependencies from Brewfile
install-brewfile:
    #!/usr/bin/env bash
    set -euo pipefail
    brew bundle

# Configure macOS defaults
configure-macos:
    #!/usr/bin/env bash
    set -euo pipefail

    # Keyboard settings
    defaults write NSGlobalDomain NSAutomaticPeriodSubstitutionEnabled -bool false
    defaults write com.apple.HIToolbox AppleFnUsageType -int 2
    defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false

    # Application settings
    defaults write NSGlobalDomain AppleShowAllExtensions -bool true
    defaults write com.google.Chrome AppleEnableSwipeNavigateWithScrolls -bool false
    defaults write com.apple.Terminal FocusFollowsMouse -bool true
    defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false

    # Dock settings
    defaults write com.apple.dock show-recents -bool false

    # System settings (requires sudo)
    sudo defaults write /Library/Preferences/com.apple.SoftwareUpdate AutomaticCheckEnabled -int 1

    # Touchpad settings
    defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1

    # Keyboard repeat rate
    defaults write NSGlobalDomain InitialKeyRepeat -int 10
    defaults write NSGlobalDomain KeyRepeat -int 2

    # Sudo with Touch ID
    # @if ! grep -q "pam_tid.so" /etc/pam.d/sudo; then \
    #     echo "Configuring Touch ID for sudo..."; \
    #     echo "auth       sufficient     pam_tid.so" | sudo tee /tmp/pam_tid_line > /dev/null; \
    #     sudo sed -i "" "2r /tmp/pam_tid_line" /etc/pam.d/sudo; \
    #     rm /tmp/pam_tid_line; \
    # fi

# Bootstrap environment (install packages, casks, and configure macOS)
bootstrap: xcode-command-line-tools install-brewfile configure-macos

# Install dotfiles
install: stow bootstrap

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
