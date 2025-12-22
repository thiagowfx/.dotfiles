#!/bin/zsh
# Set zsh prompt.

# Customize grml prompt.
#   Two lines instead of one
zstyle ':prompt:grml:left:setup' items rc change-root at host path vcs newline percent
