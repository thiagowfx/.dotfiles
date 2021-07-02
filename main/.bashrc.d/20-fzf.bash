# fzf: Fuzzy File Finder

if hash fzf >/dev/null 2>&1; then
	# fzf config - arch
	src_files /usr/share/fzf/{completion,key-bindings}.bash

	# fzf config - debian
	# https://packages.debian.org/sid/amd64/fzf/filelist
	src_files /usr/share/doc/fzf/examples/{completion,key-bindings}.bash
fi
