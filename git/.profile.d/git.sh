# __git_ps1 prompt customization: https://wiki.archlinux.org/title/Git
if hash git >/dev/null 2>&1; then
	GIT_PS1_SHOWDIRTYSTATE=1

	# arch
	src_files "/usr/share/git/git-prompt.sh"

	# debian: https://packages.debian.org/sid/amd64/git/filelist
	src_files "/usr/lib/git-core/git-sh-prompt"
fi

# git-diff-highlight: arch, debian
# alpine has /usr/bin/diff-highlight out-of-the-box
path_munge "/usr/share/git/diff-highlight" "/usr/share/doc/git/contrib/diff-highlight"
