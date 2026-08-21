# This file is managed with cog.
#
# To regenerate it, run:
#   cog -r Brewfile

# Taps
# (uses brew tap)
# [[[cog
import subprocess
import cog
taps = subprocess.check_output(['brew', 'tap']).decode().strip().split('\n')
taps.sort()
for tap in taps:
    cog.outl(f'tap "{tap}"')
# ]]]
tap "atlassian/acli"
tap "cloudflare/cloudflare"
tap "hashicorp/tap"
tap "terraform-linters/tap"
tap "thiagowfx/pancake"
tap "thiagowfx/taps"
# [[[end]]]

# Brew packages
# (uses brew leaves --installed-on-request to list only explicitly requested packages)
# Alternative: brew bundle dump (includes transitive dependencies)
# [[[cog
import subprocess
import cog
packages = subprocess.check_output(['brew', 'leaves', '--installed-on-request']).decode().strip().split('\n')
packages.sort()
for pkg in packages:
    cog.outl(f'brew "{pkg}"')
# ]]]
brew "ack"
brew "actionlint"
brew "argocd"
brew "atlassian/acli/acli"
brew "atool"
brew "atuin"
brew "awscli"
brew "azure-cli"
brew "bash"
brew "bash-completion@2"
brew "bkt"
brew "btop"
brew "ccusage"
brew "checkov"
brew "clang-format"
brew "codebook-lsp"
brew "codespell"
brew "cogapp"
brew "cookiecutter"
brew "coreutils"
brew "diffoscope"
brew "difftastic"
brew "direnv"
brew "diskonaut"
brew "dockerfmt"
brew "dos2unix"
brew "entr"
brew "expect"
brew "eza"
brew "fastfetch"
brew "fd"
brew "flock"
brew "folderify"
brew "fpp"
brew "fx"
brew "fzf"
brew "gh"
brew "ghostscript"
brew "git"
brew "git-delta"
brew "gitui"
brew "gnu-sed"
brew "graphviz"
brew "htop"
brew "httpie"
brew "hugo"
brew "icdiff"
brew "imagemagick"
brew "ipython"
brew "jj"
brew "jq"
brew "jsonlint"
brew "just"
brew "less"
brew "lesspipe"
brew "lf"
brew "llm"
brew "ls-lint"
brew "macos-trash"
brew "make"
brew "mas"
brew "midnight-commander"
brew "miller"
brew "mise"
brew "mole"
brew "moreutils"
brew "mr"
brew "mtr"
brew "ncdu"
brew "neovim"
brew "opencode"
brew "optipng"
brew "pi-coding-agent"
brew "pngcrush"
brew "poppler"
brew "prek"
brew "proselint"
brew "ranger"
brew "rename"
brew "ripgrep-all"
brew "ruff"
brew "starship"
brew "stow"
brew "tailspin"
brew "thiagowfx/pancake/pancake"
brew "tig"
brew "tmux"
brew "tmuxinator"
brew "tree"
brew "tree-sitter-cli"
brew "ty"
brew "typst"
brew "uv"
brew "vim"
brew "watch"
brew "watchexec"
brew "wget"
brew "worktrunk"
brew "yamlfmt"
brew "yamllint"
brew "yq"
brew "zizmor"
brew "zoxide"
brew "zsh"
brew "zsh-completions"
# [[[end]]]

# Brew Casks
# (uses brew list --cask)
# [[[cog
import subprocess
import cog
casks = subprocess.check_output(['brew', 'list', '--cask']).decode().strip().split('\n')
casks.sort()
for cask in casks:
    cog.outl(f'cask "{cask}"')
# ]]]
cask "1password"
cask "1password-cli"
cask "anki"
cask "brave-browser"
cask "bruno"
cask "burp-suite"
cask "calibre"
cask "claude-code"
cask "cmux"
cask "codex"
cask "duckduckgo"
cask "element"
cask "espanso"
cask "font-cascadia-code"
cask "font-commit-mono"
cask "font-hermit"
cask "font-ibm-plex-mono"
cask "ghostty"
cask "google-drive"
cask "heynote"
cask "hiddenbar"
cask "jellyfin"
cask "karabiner-elements"
cask "little-snitch"
cask "logseq"
cask "maccy"
cask "obsidian"
cask "postico"
cask "qbittorrent"
cask "raycast"
cask "rectangle"
cask "shottr"
cask "slack"
cask "spotify"
cask "swiftbar"
cask "tailscale-app"
cask "telegram-desktop"
cask "visual-studio-code"
cask "vlc"
cask "whatsapp"
cask "windscribe"
cask "zed"
# [[[end]]]
