#!/bin/bash

# pacman from Arch Linux
if hash pacman >/dev/null 2>&1; then
	# command-not-found hook
	src_files "/usr/share/doc/pkgfile/command-not-found.bash"
fi
