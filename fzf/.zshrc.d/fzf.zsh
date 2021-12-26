#!/bin/zsh

# fzf: fuzzy file finder
if (( $+commands[fzf] )); then
	# alpine, arch
	src_files /usr/share/fzf/{completion,key-bindings}.zsh

	# debian: https://packages.debian.org/sid/amd64/fzf/filelist
	src_files /usr/share/doc/fzf/examples/{completion,key-bindings}.zsh
fi
