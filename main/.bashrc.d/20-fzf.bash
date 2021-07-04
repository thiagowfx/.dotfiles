#!/bin/bash
# fzf: Fuzzy File Finder

if hash fzf >/dev/null 2>&1; then
	# arch
	src_files /usr/share/fzf/{completion,key-bindings}.bash

	# debian: https://packages.debian.org/sid/amd64/fzf/filelist
	src_files /usr/share/doc/fzf/examples/{completion,key-bindings}.bash
fi
