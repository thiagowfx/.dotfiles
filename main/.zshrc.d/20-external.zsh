#!/bin/zsh

# fzf: Fuzzy File Finder
if (( $+commands[fzf] )); then
	# fzf config - arch
	src_files /usr/share/fzf/{completion,key-bindings}.zsh

	# fzf config - debian
	# https://packages.debian.org/sid/amd64/fzf/filelist
	src_files /usr/share/doc/fzf/examples/{completion,key-bindings}.zsh
fi

# pacman from Arch Linux
if (( $+commands[pacman] )); then
	# command-not-found hook
	src_files "/usr/share/doc/pkgfile/command-not-found.zsh"
fi
