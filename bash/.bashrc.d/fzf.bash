#!/bin/bash

# fzf: fuzzy file finder
if hash fzf >/dev/null 2>&1; then
	# alpine/arch, debian
	src_files {/usr/share/fzf,/usr/share/doc/fzf/examples}/{completion,key-bindings}.bash

	# brew
	if hash brew >/dev/null 2>&1; then
		src_files "$(brew --prefix)"/opt/fzf/shell/{completion,key-bindings}.bash
	fi
fi
