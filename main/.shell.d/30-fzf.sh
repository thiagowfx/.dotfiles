# fzf: Fuzzy File Finder

if hash fzf >/dev/null 2>&1; then
	# fzf default config
	src_files "/usr/share/doc/fzf/examples/key-bindings.bash"

	# fzf cd: M-c
	add_env FZF_ALT_C_OPTS "--preview 'tree -C {} | head -100'" fzf

	# fzf history: C-r
	add_env FZF_CTRL_R_OPTS "--preview-window up:2:wrap --preview 'echo {}'"

	# fzf select: C-t
	add_env FZF_CTRL_T_OPTS "--preview '(highlight -O ansi {} 2>/dev/null || cat {} || tree -C {}) 2>/dev/null | head -100'"
fi
