#!/usr/bin/env just --justfile
# https://github.com/casey/just

packages := "ack alacritty apk atuin bash cmux gh ghostty git gitui hg i3 iterm2 jj lf logseq mc mise mole mr nvim opencode pacman pi profile ranger screen ssh starship sway tmux vim vscode worktrunk x11 zed zsh"
# claude is stowed without folding so corp-managed skills (from ~/.dotfiles_corp)
# can be symlinked into the same ~/.claude/skills dir without this repo seeing them.
packages_no_folding := "claude espanso"
packages_no_stow := "docs misc obsidian ssh_auto_tmux vendor"
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
        [cmux]="cmux"
        # The espanso package only ships the macOS config path
        # (Library/Application Support/espanso); Linux espanso reads
        # ~/.config/espanso, so gate on the .app rather than the binary.
        [espanso]="/Applications/Espanso.app/Contents/MacOS/espanso"
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
        [mise]="mise"
        [mole]="mole"
        [mr]="mr"
        [nvim]="nvim"
        [opencode]="opencode"
        [pacman]="pacman"
        [pi]="pi"
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
        [worktrunk]="wt"
        [x11]="X"
        [zed]="zed"
        [zsh]="zsh"
        # keep-sorted end
    )

    # The loops below warn when a package has no mapping; this is the missing
    # inverse. A mapping without a package is either a placeholder for config
    # not written yet (swiftbar) or a leftover from a deleted package, so warn
    # rather than fail.
    all_packages=" {{ packages }} {{ packages_no_folding }} "
    for pkg in "${!package_binaries[@]}"; do
        if [[ "$all_packages" != *" $pkg "* ]]; then
            echo "Warning: binary mapping for '$pkg', which is in no package list" >&2
        fi
    done

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

[doc('Check for dangling symlinks owned by this repository')]
[group('stow')]
stow-lint:
    #!/usr/bin/env bash
    set -euo pipefail

    repo={{ _dotfiles_dir }}
    target={{ target_dir }}
    repo_name="${repo##*/}"
    broken=0
    declare -A seen_links=()

    check_link() {
        local link="$1" link_target resolved_target
        [[ -n "${seen_links[$link]:-}" ]] && return
        seen_links[$link]=1

        link_target=$(readlink "$link")
        if [[ "$link_target" = /* ]]; then
            resolved_target=$(realpath -m "$link_target")
        else
            resolved_target=$(realpath -m "${link%/*}/$link_target")
        fi

        if [[ "$resolved_target" == "$repo/"* && ! -e "$link" ]]; then
            printf 'Broken stow link: %s -> %s\n' "$link" "$link_target" >&2
            broken=1
        fi
    }

    while IFS= read -r -d '' link; do
        check_link "$link"
    done < <(find "$target" -xdev -path "$repo" -prune -o -maxdepth 4 \
        -type l -lname "*$repo_name*" -print0 2>/dev/null)

    declare -A deep_roots=()
    for package in {{ packages }} {{ packages_no_folding }}; do
        while IFS= read -r -d '' source_dir; do
            relative_dir="${source_dir#"$repo/$package/"}"
            deep_roots["$target/$relative_dir"]=1
        done < <(find "$repo/$package" -mindepth 4 -maxdepth 4 -type d -print0)
    done

    for root in "${!deep_roots[@]}"; do
        [[ -e "$root" || -L "$root" ]] || continue
        while IFS= read -r -d '' link; do
            check_link "$link"
        done < <(find "$root" -xdev -type l -lname "*$repo_name*" -print0 2>/dev/null)
    done

    if (( broken )); then
        exit 1
    fi

    echo "No broken stow links"

[doc('Remove all symlinks')]
[group('stow')]
unstow:
    stow -t {{ target_dir }} -d {{ _dotfiles_dir }} --delete {{ packages }} {{ packages_no_folding }}

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

[doc('Install tools declared in global mise config')]
[group('install')]
mise-install:
    mise install

[doc('Install dotfiles, bootstrap environment, and install mise tools')]
[group('install')]
install: bootstrap stow mise-install

[doc('Update git submodules, Pi packages, prek hooks, and upstream files')]
[group('update')]
update: update-git update-pi update-prek sync-upstream

[doc('Update git submodules')]
[group('update')]
update-git:
    git submodule update --force --remote --jobs "$(nproc)"

[doc('Update pinned Pi packages')]
[group('update')]
update-pi:
    #!/usr/bin/env bash
    set -euo pipefail

    settings="pi/.pi/agent/settings.json"
    updates="{}"
    while IFS= read -r spec; do
        if [[ "$spec" =~ ^npm:((@[^/]+/[^@]+)|([^@]+))(@[^@]+)?$ ]]; then
            package="${BASH_REMATCH[1]}"
        else
            echo "Invalid npm package spec: $spec" >&2
            exit 1
        fi

        latest="$(npm view "$package@latest" version)"
        pinned="npm:$package@$latest"
        updates="$(jq -cn \
            --argjson updates "$updates" \
            --arg spec "$spec" \
            --arg pinned "$pinned" \
            '$updates + {($spec): $pinned}')"
        echo "✓ $pinned"
    done < <(jq -r '.packages[] | select(type == "string" and startswith("npm:"))' "$settings")

    while IFS= read -r spec; do
        if [[ "$spec" =~ ^git:(.+)@([^@/]+)$ ]]; then
            repository="${BASH_REMATCH[1]}"
            revision="${BASH_REMATCH[2]}"
        else
            echo "Invalid pinned git package spec: $spec" >&2
            exit 1
        fi

        if [[ "$repository" != *://* && "$repository" != git@* ]]; then
            repository="https://$repository"
        fi
        if [[ "$revision" =~ ^[0-9a-fA-F]{7,40}$ ]]; then
            latest="$(git ls-remote "$repository" HEAD | awk 'NR == 1 {print substr($1, 1, 12)}')"
            if [[ -z "$latest" ]]; then
                echo "No HEAD commit found for $spec" >&2
                exit 1
            fi
        else
            latest="$(git ls-remote --tags --refs "$repository" \
                | awk -F/ '{print $3}' \
                | { grep -E '^(v)?[0-9]+(\.[0-9]+){0,2}$' || true; } \
                | sort -V \
                | tail -n 1)"
            if [[ -z "$latest" ]]; then
                echo "No SemVer tags found for $spec" >&2
                exit 1
            fi
        fi

        pinned="${spec%@*}@$latest"
        updates="$(jq -cn \
            --argjson updates "$updates" \
            --arg spec "$spec" \
            --arg pinned "$pinned" \
            '$updates + {($spec): $pinned}')"
        echo "✓ $pinned"
    done < <(jq -r '.packages[] | select(type == "object" and (.source | type == "string") and (.source | test("^git:.+@[^@/]+$"))) | .source' "$settings")

    updated_settings="$(jq --argjson updates "$updates" \
        '.packages |= map(
            if type == "string" then $updates[.] // .
            elif type == "object" then .source = ($updates[.source] // .source)
            else .
            end
        )' "$settings")"
    printf '%s\n' "$updated_settings" > "$settings"

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
        ["vendor/schemas/pi-theme.json"]="https://raw.githubusercontent.com/earendil-works/pi/main/packages/coding-agent/src/modes/interactive/theme/theme-schema.json"
        ["vendor/schemas/yamllint.json"]="https://www.schemastore.org/yamllint.json"
        # keep-sorted end
    )

    for local_file in "${!files[@]}"; do
        curl -fsSL "${files[$local_file]}" -o "$local_file"
        echo "✓ $local_file"
    done
