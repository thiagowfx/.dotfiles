#!/bin/sh
# Sensible command defaults for both bash and zsh.

# color on
alias diff="diff -uN --color=auto"
alias grep="grep --color=auto"

# verbose on
alias ls="ls -Fh --color=auto"
alias la="ls -la --color=auto"
alias l="ls -l --color=auto"
alias ll="l"

# eza: modern drop-in replacement for ls
if hash eza >/dev/null 2>&1; then
        alias ls="eza -F --group-directories-first"
fi

# https://direnv.net/
if hash direnv >/dev/null 2>&1; then
        alias de="direnv edit || direnv edit ."
fi

# misspellings
alias t=git
alias it=git
alias gi=git
alias gt=git
alias gti=git
alias sl=ls

alias cdg='cd "$(git root)"'

# shortcuts
alias k=kubectl

# muscle memory
alias unstow="stow -D"
