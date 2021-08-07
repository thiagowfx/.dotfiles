#!/bin/zsh

# Disable unneeded features from grml-zsh-config.
# https://grml.org/zsh/grmlzshrc.html
MAILCHECK=0

# Fish shell-like syntax highlighting for Zsh.
src_files "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Fish shell-like autosuggestions for zsh.
# Suggest commands as you type based on history and completions.
#   Default: history only.
#   Use C-e or C-f to accept a suggestion.
src_files "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Fish shell-like history search (up arrow).
# Load it after zsh-syntax-highlighting.
src_files "$HOME/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh"
bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down