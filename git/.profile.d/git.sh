# __git_ps1 upstream prompt customization: https://wiki.archlinux.org/title/Git
if hash git >/dev/null 2>&1; then
	GIT_PS1_SHOWCOLORHINTS=1
	GIT_PS1_SHOWDIRTYSTATE=1
	GIT_PS1_SHOWSTASHSTATE=1

	# alpine:nil, arch, debian
	src_files "/usr/share/git/git-prompt.sh" "/usr/lib/git-core/git-sh-prompt"

	# git-diff-highlight: alpine:/usr/bin/, arch, debian
	path_munge "/usr/share/git/diff-highlight" "/usr/share/doc/git/contrib/diff-highlight"

	# brew
	if hash brew >/dev/null 2>&1; then
		src_files "$(brew --prefix)"/etc/bash_completion.d/git-prompt.sh
		path_munge "$(brew --prefix)"/share/git-core/contrib/diff-highlight
	fi

fi
