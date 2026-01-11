#!/bin/zsh

# Fish shell-like syntax highlighting for Zsh.
src_files "$HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Fish shell-like autosuggestions for zsh.
# Suggest commands as you type based on history and completions.
#   Use C-e or C-f to accept a suggestion.
src_files "$HOME/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# homebrew
(( $+commands[brew] )) && FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

# a smarter cd command - z
# https://github.com/ajeetdsouza/zoxide
(( $+commands[zoxide] )) && eval "$(zoxide init zsh)"

# fzf: fuzzy file finder
# https://github.com/junegunn/fzf#setting-up-shell-integration
(( $+commands[fzf] )) && source <(fzf --zsh)

# atuin: https://docs.atuin.sh/
(( $+commands[atuin] )) && eval "$(atuin init zsh --disable-up-arrow)"
