# Set linux tty/console theme
# /base16/monokai.dark from https://terminal.love/
if [ "$TERM" = "linux" ]; then
	echo -e "
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

