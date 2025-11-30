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
if (( $+commands[fzf] )); then
        # alpine/arch, debian
        src_files {/usr/share/fzf,/usr/share/doc/fzf/examples}/{completion,key-bindings}.zsh

        # brew
        if (( $+commands[brew] )); then
                src_files "$(brew --prefix)"/opt/fzf/shell/{completion,key-bindings}.zsh
        fi
fi

# atuin: https://docs.atuin.sh/
(( $+commands[atuin] )) && eval "$(atuin init zsh)"
