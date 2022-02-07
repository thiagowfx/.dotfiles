#!/bin/bash

# fzf: fuzzy file finder
if hash fzf >/dev/null 2>&1; then
	# alpine, arch
	src_files /usr/share/fzf/{completion,key-bindings}.bash

	# debian: https://packages.debian.org/sid/amd64/fzf/filelist
	src_files /usr/share/doc/fzf/examples/{completion,key-bindings}.bash

	# nix
	hash fzf-share >/dev/null 2>&1 && src_files "$(fzf-share)"/{completion,key-bindings}.bash
fi

