# Custom user environment variables

# Sensible $PATH
path_munge "$HOME/.bin"
path_munge "$HOME/bin"

# Colorize CLI output when supported.
add_env CLICOLOR 1

# Set text editors.
add_env EDITOR "vim" vim
add_env VISUAL "vim" vim

# Sensible defaults for less: --raw-control-chars --ignore-case
add_env LESS "-R -i" less

# Display progress percentage in man pages: https://unix.stackexchange.com/a/329092/41858
add_env MANPAGER "less -R -s -M +Gg" less

# Colored man pages: https://wiki.archlinux.org/title/Color_output_in_console#man
if hash less >/dev/null 2>&1 && [ "$(less -V | head -n 1 | cut -f2 -d' ')" -ge 580 ]; then
	add_env MANPAGER "less -R -s -M +Gg --use-color -Dd+r -Du+b" less
fi

if hash fzf >/dev/null 2>&1; then
	# fzf history: C-r
	add_env FZF_CTRL_R_OPTS "--preview-window up:2:wrap --preview 'echo {}'"

	# fzf select: C-t
	add_env FZF_CTRL_T_OPTS "--preview '(highlight -O ansi {} 2>/dev/null || cat {} || tree -C {}) 2>/dev/null | head -50'"

	# fzf cd: M-c
	add_env FZF_ALT_C_OPTS "--preview 'tree -C {} | head -50'"
fi

# Set linux console theme
# /base16/monokai.dark from https://terminal.love/
if [ "$TERM" = "linux" ]; then
	/bin/echo -e "
	\e]P0272822
	\e]P1f92672
	\e]P2a6e22e
	\e]P3f4bf75
	\e]P466d9ef
	\e]P5ae81ff
	\e]P6a1efe4
	\e]P7f8f8f2
	\e]P875715e
	\e]P9f92672
	\e]PAa6e22e
	\e]PBf4bf75
	\e]PC66d9ef
	\e]PDae81ff
	\e]PEa1efe4
	\e]PFf9f8f5
	"
	# clear artifacts
	clear
fi
