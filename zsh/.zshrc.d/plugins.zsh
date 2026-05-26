#!/bin/zsh

# atuin: https://docs.atuin.sh/
(( $+commands[atuin] )) && eval "$(atuin init zsh --disable-up-arrow)"

# homebrew
(( $+commands[brew] )) && FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

# fzf: fuzzy file finder
# https://github.com/junegunn/fzf#setting-up-shell-integration
(( $+commands[fzf] )) && source <(fzf --zsh)

# worktrunk: https://worktrunk.dev/
(( $+commands[wt] )) && eval "$(wt config shell init zsh)"

# a smarter cd command — z
# https://github.com/ajeetdsouza/zoxide
(( $+commands[zoxide] )) && eval "$(zoxide init zsh)"

# Fish shell-like autosuggestions for zsh.
# Suggest commands as you type based on history and completions.
#   Use C-e or C-f to accept a suggestion.
src_files "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# Fish shell-like syntax highlighting for Zsh.
# MUST be sourced last — wraps ZLE widgets and breaks if later plugins
# (fzf, atuin, etc.) redefine the widgets it wraps.
src_files "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
