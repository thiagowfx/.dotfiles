#!/bin/zsh
# https://direnv.net/
if (( $+commands[direnv] )); then
	eval "$(direnv hook zsh)"
fi
