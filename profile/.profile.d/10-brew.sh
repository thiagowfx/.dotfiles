# Homebrew: The Missing Package Manager for macOS (or Linux)
# https://brew.sh

# Use `brew --prefix` for the homebrew prefix:
#   /usr/local for Intel Macs, /usr/local/bin is in $PATH out-of-the-box
#   /opt/homebrew for Apple Silicon / ARM Macs, need to add /opt/homebrew/bin to $PATH
#
# The following line is idempotent.
[ -z "$HOMEBREW_PREFIX" ] && [ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"

# command-not-found hook
# setup: brew tap "homebrew/command-not-found"
# https://github.com/Homebrew/homebrew-command-not-found
hash brew >/dev/null 2>&1 && src_files "$(brew --prefix)/Library/Taps/homebrew/homebrew-command-not-found/handler.sh"

# GNU coreutils
path_munge "/opt/homebrew/opt/coreutils/libexec/gnubin"
# GNU sed
path_munge "/opt/homebrew/opt/gnu-sed/libexec/gnubin"

# Node 20
path_munge "/opt/homebrew/opt/node@20/bin"
