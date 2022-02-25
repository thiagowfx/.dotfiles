#!/bin/bash

# fzf: fuzzy file finder
if hash fzf >/dev/null 2>&1; then
	# alpine/arch, debian, nix
	src_files {/usr/share/fzf,/usr/share/doc/fzf/examples,"$HOME"/.nix-profile/share/fzf}/{completion,key-bindings}.bash
fi

