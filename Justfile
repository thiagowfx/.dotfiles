#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack alacritty amp apk atuin bash claude gemini gh ghostty git gitui hg i3 iterm2 jj lf logseq mc mr nvim opencode pacman profile ranger screen ssh starship sway tmux vim vscode x11 zed zsh"
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

    # Map package names to their binaries
    declare -A package_binaries=(
        # keep-sorted start
        [ack]="ack"
        [alacritty]="alacritty"
        [amp]="amp"
        [apk]="apk"
        [atuin]="atuin"
        [bash]="bash"
        [claude]="claude"
        [espanso]="espanso"
        [gemini]="gemini"
        [gh]="gh"
        [ghostty]="ghostty"
        [git]="git"
        [gitui]="gitui"
        [hg]="hg"
        [i3]="i3"
        [iterm2]="/Applications/iTerm.app/Contents/MacOS/iTerm2"
        [jj]="jj"
        [lf]="lf"
        [logseq]="/Applications/LogSeq.app/Contents/MacOS/LogSeq"
        [mc]="mc"
        [mr]="mr"
        [nvim]="nvim"
        [opencode]="opencode"
        [pacman]="pacman"
        [profile]="sh"
        [ranger]="ranger"
        [screen]="screen"
        [ssh]="ssh"
        [starship]="starship"
        [sway]="sway"
        [swiftbar]="/Applications/SwiftBar.app/Contents/MacOS/SwiftBar"
        [tmux]="tmux"
        [vim]="vim"
        [vscode]="code"
        [x11]="X"
        [zed]="zed"
        [zsh]="zsh"
        # keep-sorted end
    )

    # Stow packages with regular folding
    stow_packages=""
    for pkg in {{ packages }}; do
        binary="${package_binaries[$pkg]:-}"
        if [[ -z "$binary" ]]; then
            echo "Warning: No binary mapping for package '$pkg', skipping" >&2
            continue
        fi
        if [[ -f "$binary" ]] || command -v "$binary" &> /dev/null; then
            echo "Stowing '$pkg'"
            stow_packages="$stow_packages $pkg"
        else
            echo "Skipping '$pkg' (binary '$binary' not found)"
        fi
    done
    if [[ -n "$stow_packages" ]]; then
        stow -v -t {{ target_dir }} -d {{ _dotfiles_dir }} $stow_packages
    fi

    # Stow packages with no folding
    stow_packages_no_folding=""
    for pkg in {{ packages_no_folding }}; do
        binary="${package_binaries[$pkg]:-}"
        if [[ -z "$binary" ]]; then
            echo "Warning: No binary mapping for package '$pkg', skipping" >&2
            continue
        fi
        if [[ -f "$binary" ]] || command -v "$binary" &> /dev/null; then
            echo "Stowing '$pkg' (no folding)"
            stow_packages_no_folding="$stow_packages_no_folding $pkg"
        else
            echo "Skipping '$pkg' (binary '$binary' not found)"
        fi
    done
    if [[ -n "$stow_packages_no_folding" ]]; then
        stow -v -t {{ target_dir }} -d {{ _dotfiles_dir }} --no-folding $stow_packages_no_folding
    fi

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
    defaults write NSGlobalDomain KeyRepeat -int 1

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
    mkdir -p vendor/schemas

    # Define schema mappings (filename -> URL)
    declare -A schemas=(
         ["espanso-config.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/config.schema.json"
         ["espanso-match.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/match.schema.json"
         ["yamllint.json"]="https://json.schemastore.org/yamllint.json"
     )

    failed=0
    succeeded=0

    # Download each schema
    for filename in "${!schemas[@]}"; do
        url="${schemas[$filename]}"
        output_path="vendor/schemas/$filename"

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
