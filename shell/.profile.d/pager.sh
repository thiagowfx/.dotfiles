if hash less >/dev/null 2>&1; then
	# Set sensible defaults for less.
	# https://stackoverflow.com/a/14118014/1745064
	export LESS="-FR"

	# Enhanced man pages
	#   Color: https://wiki.archlinux.org/title/Color_output_in_console#man
	#   Progress percentage: https://unix.stackexchange.com/a/329092/41858
	export MANPAGER="less -R -s -M +Gg"

	if [ "$(less -V | head -n 1 | cut -f2 -d' ' | xargs printf '%.0f')" -ge 580 ]; then
		export MANPAGER="${MANPAGER} --use-color -Dd+r -Du+b"
	else
		man() {
			LESS_TERMCAP_md=$'\e[01;31m' \
				LESS_TERMCAP_me=$'\e[0m' \
				LESS_TERMCAP_se=$'\e[0m' \
				LESS_TERMCAP_so=$'\e[01;44;33m' \
				LESS_TERMCAP_ue=$'\e[0m' \
				LESS_TERMCAP_us=$'\e[01;34m' \
				command man "$@"
			}
	fi
fi

