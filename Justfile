#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack alacritty apk atuin bash claude gh ghostty git gitui hg i3 iterm2 jj lf logseq mc mole mr nvim opencode pacman profile ranger screen ssh starship sway tmux vim vscode wt x11 zed zsh"
packages_no_folding := "espanso swiftbar"
packages_no_stow := "misc plans ssh_auto_tmux vendor"
[private]
_dotfiles_dir := justfile_directory()
target_dir := env_var("HOME")

# List available commands
@_list:
    just --list

[doc('Stow all packages')]
[group('stow')]
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
        [apk]="apk"
        [atuin]="atuin"
        [bash]="bash"
        [claude]="claude"
        [espanso]="espanso"
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
        [mole]="mole"
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
        [wt]="wt"
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

[doc('Check for dangling symlinks')]
[group('stow')]
stow-lint:
    chkstow -t {{ target_dir }}

[doc('Remove all symlinks')]
[group('stow')]
unstow:
    stow -t {{ target_dir }} -d {{ _dotfiles_dir }} --delete {{ packages }}

[doc('Install Xcode Command Line Tools')]
[group('bootstrap')]
xcode-command-line-tools:
    #!/usr/bin/env bash
    set -euo pipefail

    if ! command -v xcode-select &> /dev/null; then
        echo "Installing Xcode Command Line Tools..."
        xcode-select --install
    else
        echo "Xcode Command Line Tools already installed"
    fi

[doc('Install dependencies from Brewfile (Homebrew packages and casks)')]
[group('bootstrap')]
install-brewfile:
    #!/usr/bin/env bash
    set -euo pipefail
    # Strip cog markers and Python code before passing to brew bundle
    sed '/# \[\[\[cog/,/# \]\]\]/d; /# \[\[\[end\]\]\]/d' Brewfile | brew bundle --file=-

[doc('Configure macOS defaults (keyboard, dock, security, etc.)')]
[group('bootstrap')]
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

[doc('Bootstrap environment (install packages, casks, and configure macOS)')]
[group('bootstrap')]
bootstrap: xcode-command-line-tools install-brewfile configure-macos

[doc('Install dotfiles (stow packages and bootstrap environment)')]
[group('install')]
install: stow bootstrap

[doc('Update git submodules, prek hooks, and upstream files')]
[group('update')]
update: update-git update-prek sync-upstream

[doc('Update git submodules')]
[group('update')]
update-git:
    git submodule update --force --remote --jobs "$(nproc)"

[doc('Update prek hooks and run all hooks')]
[group('update')]
update-prek:
    prek autoupdate --freeze --jobs "$(nproc)" && prek run --all-files

[doc('Overwrite vendored files with their upstream sources (review with git diff)')]
[group('update')]
sync-upstream:
    #!/usr/bin/env bash
    set -euo pipefail

    declare -A files=(
        # keep-sorted start
        ["vendor/gitui/key_bindings.ron"]="https://raw.githubusercontent.com/gitui-org/gitui/master/vim_style_key_config.ron"
        ["vendor/grml-etc-core/etc/zsh/zshrc"]="https://raw.githubusercontent.com/grml/grml-etc-core/master/etc/zsh/zshrc"
        ["vendor/schemas/espanso-config.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/config.schema.json"
        ["vendor/schemas/espanso-match.schema.json"]="https://raw.githubusercontent.com/espanso/espanso/refs/heads/dev/schemas/match.schema.json"
        ["vendor/schemas/yamllint.json"]="https://www.schemastore.org/yamllint.json"
        # keep-sorted end
    )

    for local_file in "${!files[@]}"; do
        curl -fsSL "${files[$local_file]}" -o "$local_file"
        echo "✓ $local_file"
    done
