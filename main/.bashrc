#!/bin/bash

# bash startup file

# shell is non-interactive; be done now!
[[ $- != *i* ]] && return

# History: Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"

# History: Set unlimited size. Otherwise set to 50000.
HISTSIZE=
HISTFILESIZE=

# History: Use standard ISO 8601 timestamp.
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='[%F %T] '

# History: Set eternal history file.
# Uncomment this line to use the ~/.bash_history default.
HISTFILE="$HOME/.bash_eternal_history"

# History: Default commands not to record.
HISTIGNORE="dirs:ls:tree:vdir:clear:history:pwd"

# Prevent file overwrite on stdout redirection.
# Use `>|` to force redirection to an existing file.
# Only applies to interactive shells.
set -o noclobber

# Make C-r followed by C-s work in reverse history search.
stty -ixon

# Complete filenames after flag arguments.
# https://stackoverflow.com/a/33740951/1745064
#
#   $ foo --config=$HOME/.b| <TAB> would expand .bashrc.
complete -D -o default

# Enable history expansion with space.
# Typing !!<SPC> will replace the !! with the last command.
bind Space:magic-space

# Prepend cd to directory names automatically.
shopt -s autocd

# Correct spelling errors in arguments supplied to cd.
shopt -s cdspell

# Correct spelling errors during tab-completion.
shopt -s dirspell

# Turn on recursive globbing: Enables ** to recurse all directories.
shopt -s globstar

# Append to the history file, do not overwrite it.
shopt -s histappend

# If set, and Readline is being used, a user is given the opportunity to re-edit a failed history substitution.
shopt -s histreedit

# If set, and Readline is being used, the results of history substitution are not immediately passed to the shell parser.
# Instead, the resulting line is loaded into the Readline editing buffer, allowing further modification.
shopt -s histverify

# Sources the given files and directories (recursively) if they exist.
src_files() {
	for f in "$@"; do
		# Source directories recursively
		if [ -d "$f" ]; then
			src_files "$f"/* || true
		# Source files
		elif [ -f "$f" ]; then
			# shellcheck source=/dev/null
			. "$f" || true
		fi
	done
}

# Load user scripts and functions if any.
# Order is important.
src_files "$HOME/.bashrc.d"

# add user scripts to $PATH
pathmunge "$HOME/.bin"
pathmunge "$HOME/bin"

# Load corp config if any.
src_files "$HOME/.bashrc_corp"
