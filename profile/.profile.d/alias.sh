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
if command -v eza >/dev/null 2>&1; then
        alias ls="eza -F --group-directories-first"
fi

# https://direnv.net/
if command -v direnv >/dev/null 2>&1; then
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

# nvim as vim (must be after 10_brew.sh adds Homebrew to PATH)
command -v nvim >/dev/null 2>&1 && export EDITOR="nvim" VISUAL="nvim" && alias vim=nvim
