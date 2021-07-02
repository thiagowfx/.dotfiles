#!/bin/zsh

# Source base shell functions.
[ -r ~/.shellrc ] && . ~/.shellrc

# Load user scripts and functions if existing. Order is important.
src_files "$HOME/.shell.d" "$HOME/.zshrc.d"

# Load corp configs if any.
src_files "$HOME/.zshrc_corp"
