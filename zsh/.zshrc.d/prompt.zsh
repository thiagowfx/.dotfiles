#!/bin/zsh

# Use starship prompt if available, otherwise customize grml prompt.
if (( $+commands[starship] )); then
    prompt off
    eval "$(starship init zsh)"
else
    # Customize grml prompt: two lines, user in bold green
    zstyle ':prompt:grml:left:items:user' pre '%B%F{green}'
    zstyle ':prompt:grml:left:setup' items rc change-root user at host path vcs newline percent
fi
