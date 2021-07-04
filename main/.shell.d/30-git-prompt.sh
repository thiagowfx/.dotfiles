# git prompt

if hash git >/dev/null 2>&1; then
	# arch
	src_files "/usr/share/git/git-prompt.sh"

	# debian: https://packages.debian.org/sid/amd64/git/filelist
	src_files "/usr/lib/git-core/git-sh-prompt"
fi
