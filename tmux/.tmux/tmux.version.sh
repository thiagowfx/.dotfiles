#!/bin/bash
# https://raw.githubusercontent.com/tmux/tmux/master/CHANGES, v2.1 breaks compat

version="$(tmux -V | cut -c 6-)"

if [[ $(echo "$version >= 2.1" | bc) -eq 1 || $version == "master" ]]; then
	tmux source-file "$HOME/.tmux/tmux2.1_up.conf"
else
	tmux source-file "$HOME/.tmux/tmux2.1_down.conf"
fi

# vim: fdm=marker ft=sh
