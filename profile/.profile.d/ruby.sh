# rbenv
if hash rbenv >/dev/null 2>&1; then
	path_munge "$HOME/.rbenv/bin"
	eval "$(rbenv init -)"
fi
