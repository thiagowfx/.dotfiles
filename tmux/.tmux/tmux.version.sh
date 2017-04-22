#!/bin/sh
#
# see: https://raw.githubusercontent.com/tmux/tmux/master/CHANGES
# version 2.1 breaks compatibility
#

tmux_version="$(tmux -V | cut -c 6-)"

if [[ $(echo "$tmux_version >= 2.1" | bc) -eq 1 ]]; then
	tmux source-file "$HOME/.tmux/tmux_2.1_up.conf"
else
	tmux source-file "$HOME/.tmux/tmux_2.1_down.conf"
fi

# vim: fdm=marker ft=sh
