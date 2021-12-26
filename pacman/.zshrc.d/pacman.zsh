#!/bin/zsh

# pacman from Arch Linux
if (( $+commands[pacman] )); then
	# command-not-found hook
	src_files "/usr/share/doc/pkgfile/command-not-found.zsh"
fi

