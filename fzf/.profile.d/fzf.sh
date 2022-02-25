if hash fzf >/dev/null 2>&1; then
	# fzf history: C-r
	export FZF_CTRL_R_OPTS="--preview-window up:2:wrap --preview 'echo {}'"

	# fzf select: C-t
	export FZF_CTRL_T_OPTS="--preview '(cat {} || tree -C {}) 2>/dev/null | head -50'"
fi
