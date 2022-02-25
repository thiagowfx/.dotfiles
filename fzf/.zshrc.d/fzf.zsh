#!/bin/zsh

# fzf: fuzzy file finder
if (( $+commands[fzf] )); then
	# alpine/arch, debian, nix
	src_files {/usr/share/fzf,/usr/share/doc/fzf/examples,"$HOME"/.nix-profile/share/fzf}/{completion,key-bindings}.zsh
fi
