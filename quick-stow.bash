#!/bin/bash
#
# Quickly stows the most popular stows.
#

stows=(bash bin git hg mr tmux zsh)

while getopts ":u" opt; do
	case $opt in
		u)
			UNSTOW=1
			;;
		\?)
			echo "error: invalid option: -$OPTARG">&2
			exit 1
			;;
	esac
done

for d in ${stows[@]}; do
	if [ -z ${UNSTOW} ]; then
		stow $d
	else
		stow -D $d
	fi
done
