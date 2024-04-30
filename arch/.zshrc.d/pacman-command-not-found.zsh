#!/bin/zsh
# pacman(8) from Arch Linux command-not-found hook for zsh

if (( $+commands[pacman] )); then
	# command-not-found hook
	src_files "/usr/share/doc/pkgfile/command-not-found.zsh"
fi
