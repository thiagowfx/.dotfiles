# __git_ps1 prompt customization: https://wiki.archlinux.org/title/Git
if hash git >/dev/null 2>&1; then
	GIT_PS1_SHOWDIRTYSTATE=1

	# alpine:nil, arch, debian, nix
	src_files "/usr/share/git/git-prompt.sh" "/usr/lib/git-core/git-sh-prompt" "$HOME/.nix-profile/share/git/contrib/completion/git-prompt.sh"
fi

# git-diff-highlight: alpine:/usr/bin/diff-highlight, arch, debian, nix
path_munge "/usr/share/git/diff-highlight" "/usr/share/doc/git/contrib/diff-highlight" "$HOME/.nix-profile/share/git/contrib/diff-highlight"
