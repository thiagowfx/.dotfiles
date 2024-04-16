#!/bin/sh

# Custom user environment variables for both bash and zsh.

# Sensible $PATH should include user binary directories.
path_munge "$HOME/.bin" "$HOME/bin"

# Colorize CLI output when supported.
export CLICOLOR=1

# Set preferred text editor.
hash vim >/dev/null 2>&1 && export EDITOR="vim" VISUAL="vim"
