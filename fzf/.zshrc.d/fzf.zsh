#!/bin/zsh

# fzf: fuzzy file finder
if (( $+commands[fzf] )); then
	# alpine/arch, debian
	src_files {/usr/share/fzf,/usr/share/doc/fzf/examples}/{completion,key-bindings}.zsh

	# brew
	if (( $+commands[brew] )); then
		src_files "$(brew --prefix)"/opt/fzf/shell/{completion,key-bindings}.zsh
	fi
fi
