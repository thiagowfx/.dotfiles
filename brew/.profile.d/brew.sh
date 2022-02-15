# Homebrew: The Missing Package Manager for macOS (or Linux)
# https://brew.sh
#
# Use `brew --prefix` to refer to the homebrew prefix.
#   /usr/local for Intel Macs
#   /opt/homebrew for Apple Silicon / ARM Macs
#
# The following line is idempotent.
[ -z $HOMEBREW_PREFIX ] && [ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
