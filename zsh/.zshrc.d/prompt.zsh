#!/bin/zsh
# Set zsh prompt.

# Customize grml prompt.
#   Two lines instead of one
zstyle ':prompt:grml:left:items:user' pre '%B%F{green}'
zstyle ':prompt:grml:left:setup' items rc change-root user at host path vcs newline percent
