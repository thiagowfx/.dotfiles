#!/bin/bash

# abort on non-interactive shells
[[ $- != *i* ]] && return

# History: Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"

# History: Set unlimited size. Otherwise manually set to 50000.
HISTSIZE=
HISTFILESIZE=

# History: Use standard ISO 8601 timestamp.
#   %F is equivalent to %Y-%m-%d
#   %T is equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='[%F %T] '

# History: Default commands not to record.
HISTIGNORE="clear:history:ls:pwd:tree"

# Make C-r followed by C-s work in reverse history search.
stty -ixon

# Complete filenames after flag arguments.
#   https://stackoverflow.com/a/33740951/1745064
#
#   $ foo --config=$HOME/.b| <TAB> would expand .bashrc.
complete -D -o default

# Enable history expansion with space.
#   Typing !!<SPC> will replace !! with the last command.
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

# Source base shell functions.
# shellcheck source=/dev/null
[ -r ~/.shellrc ] && . ~/.shellrc

# Load user scripts and functions if existing. Order is important.
src_files "$HOME/.profile.d" "$HOME/.bashrc.d"

# Load corp configs if any.
src_files "$HOME/.bashrc_corp"
