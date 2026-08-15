#!/bin/sh

# Homebrew: The Missing Package Manager for macOS (or Linux)
# https://brew.sh

# Probe the known homebrew prefixes, in order:
#   /opt/homebrew for Apple Silicon / ARM Macs, need to add /opt/homebrew/bin to $PATH
#   /usr/local for Intel Macs, /usr/local/bin is in $PATH out-of-the-box
#   /home/linuxbrew/.linuxbrew for Linuxbrew
#
# brew shellenv is dedupe-safe, so it's fine to re-run on every shell startup.
# Running it unconditionally fixes PATH ordering after /etc/zprofile's
# path_helper reshuffles it (e.g. in nested login shells like dux's companion
# terminal, where HOMEBREW_PREFIX is already inherited from the parent).
# It also exports HOMEBREW_PREFIX, which the path_munge calls below rely on.
for _brew_prefix in /opt/homebrew /usr/local /home/linuxbrew/.linuxbrew "$HOME/.linuxbrew"; do
	if [ -x "$_brew_prefix/bin/brew" ]; then
		eval "$("$_brew_prefix/bin/brew" shellenv)" && export HOMEBREW_NO_ENV_HINTS=1
		break
	fi
done
unset _brew_prefix

# command-not-found hook
# brew --prefix works too
command -v brew >/dev/null 2>&1 && src_files "$(brew --repository)/Library/Homebrew/command-not-found/handler.sh"

# GNU coreutils
path_munge "${HOMEBREW_PREFIX:-/opt/homebrew}/opt/coreutils/libexec/gnubin"

# GNU sed
path_munge "${HOMEBREW_PREFIX:-/opt/homebrew}/opt/gnu-sed/libexec/gnubin"

# Trash
path_munge "${HOMEBREW_PREFIX:-/opt/homebrew}/opt/macos-trash/bin"

# Rancher Desktop: https://rancherdesktop.io
path_munge "$HOME/.rd/bin"
