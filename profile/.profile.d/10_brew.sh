#!/bin/sh

# Homebrew: The Missing Package Manager for macOS (or Linux)
# https://brew.sh

# Use `brew --prefix` for the homebrew prefix:
#   /usr/local for Intel Macs, /usr/local/bin is in $PATH out-of-the-box
#   /opt/homebrew for Apple Silicon / ARM Macs, need to add /opt/homebrew/bin to $PATH
#
# The following line is idempotent.
[ -z "$HOMEBREW_PREFIX" ] && [ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)" && export HOMEBREW_NO_ENV_HINTS=1

# command-not-found hook
# brew --prefix works too
command -v brew >/dev/null 2>&1 && src_files "$(brew --repository)/Library/Homebrew/command-not-found/handler.sh"

# GNU coreutils
path_munge "/opt/homebrew/opt/coreutils/libexec/gnubin"

# GNU sed
path_munge "/opt/homebrew/opt/gnu-sed/libexec/gnubin"

# Trash
path_munge "/opt/homebrew/opt/macos-trash/bin"

# Rancher Desktop: https://rancherdesktop.io
path_munge "$HOME/.rd/bin"
