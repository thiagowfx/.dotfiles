# __git_ps1 prompt customization: https://wiki.archlinux.org/title/Git
if hash git >/dev/null 2>&1; then
	GIT_PS1_SHOWDIRTYSTATE=1

	# arch
	src_files "/usr/share/git/git-prompt.sh"

	# debian: https://packages.debian.org/sid/amd64/git/filelist
	src_files "/usr/lib/git-core/git-sh-prompt"

	# nix
	src_files "$HOME/.nix-profile/share/git/contrib/completion/git-prompt.sh"
fi

# git-diff-highlight: arch, debian, nix
# alpine has /usr/bin/diff-highlight out-of-the-box
path_munge "/usr/share/git/diff-highlight" "/usr/share/doc/git/contrib/diff-highlight" "$HOME/.nix-profile/share/git/contrib/diff-highlight"
