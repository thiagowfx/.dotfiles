#!/bin/zsh

# Fish shell-like syntax highlighting for Zsh.
src_files "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Fish shell-like autosuggestions for zsh.
# Suggest commands as you type based on history and completions.
#   Use C-e or C-f to accept a suggestion.
src_files "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# homebrew
(( $+commands[brew] )) && FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}" && compinit

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

# a smarter cd command
# https://github.com/ajeetdsouza/zoxide
(( $+commands[zoxide] )) && eval "$(zoxide init zsh)"

# iTerm2 shell integration: https://iterm2.com/documentation-shell-integration.html
src_files "$HOME/.iterm2_shell_integration.zsh"
