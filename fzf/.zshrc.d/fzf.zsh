#!/bin/zsh

# fzf: fuzzy file finder
if (( $+commands[fzf] )); then
	# alpine/arch, debian
	src_files {/usr/share/fzf,/usr/share/doc/fzf/examples}/{completion,key-bindings}.zsh
fi
