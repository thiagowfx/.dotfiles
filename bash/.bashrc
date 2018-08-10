 # -*- shell-script -*-

if [[ $- != *i* ]] ; then
	# shell is non-interactive; be done now!
	return
fi

src_files() {
	for f in "$@"; do
		if [[ -d "$f" ]]; then
			src_files "$f"/* || true
		elif [[ -f "$f" ]]; then
			source "$f" || true
		fi
	done
}

add_env() {
	[[ "x$3" != "x" ]] && ! hash "$3" &>/dev/null && return
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
	export "$1"="$2"
}

add_paths() {
	for d in "$@"; do
		add_env PATH "$d:$PATH" "" "$d"
	done
}

add_alias() {
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return

	if [[ "x$3" != "x" ]]; then
		hash "$3" &>/dev/null && alias "$1"="$2"
	else
		hash "$2" &>/dev/null && alias "$1"="$2"
	fi
}

add_alias .. "cd .." cd
add_alias chrome "google-chrome" google-chrome
add_alias l "ls -l" ls
add_alias la "ls -al" ls
add_alias screen "screen -U" screen
add_alias sl "ls" ls
add_alias tmux "tmux -2" tmux
add_alias unstow "stow -D" stow

add_env CLICOLOR "1"
add_env EDITOR "vim" vim && add_env VISUAL "$EDITOR" "$EDITOR"

add_paths "$HOME/.bin" "$HOME/bin" # user scripts

# ignore space and ignore duplicates
HISTCONTROL="ignoreboth"
HISTSIZE=50000
HISTFILESIZE="${HISTSIZE}"
HISTIGNORE="ls:la:ll:l:tree:vdir:history:exit:prodaccess"

# bash shell options
set -o emacs
for option in autocd checkwinsize cdspell cmdhist dotglob expand_aliases extglob histappend hostcomplete nocaseglob; do
	shopt -s $option &>/dev/null
done

# bash colored man pages
man() {
	env LESS_TERMCAP_mb=$'\E[01;31m' \
		LESS_TERMCAP_md=$'\E[01;38;5;74m' \
		LESS_TERMCAP_me=$'\E[0m' \
		LESS_TERMCAP_se=$'\E[0m' \
		LESS_TERMCAP_so=$'\E[38;5;246m' \
		LESS_TERMCAP_ue=$'\E[0m' \
		LESS_TERMCAP_us=$'\E[04;38;5;146m' \
		man "$@"
}

# enable bash completion for sudo
complete -cf sudo

# linux bash completion
src_files "/etc/bash_completion" "/usr/share/bash-completion/bash_completion"

# homebrew bash completion
src_files "/usr/local/etc/bash_completion" "/usr/local/share/bash-completion/bash_completion" "/usr/local/etc/bash_completion.d"

# homebrew command-not-found hook
hash brew &>/dev/null && brew command command-not-found-init >/dev/null 2>&1 && eval "$(brew command-not-found-init)"

# pacman command-not-found hook
hash pacman &>/dev/null && src_files "/usr/share/doc/pkgfile/command-not-found.bash"

# vte.sh
src_files "/etc/profile.d/vte.sh"

# fzf
src_files "$HOME/.fzf.bash"

# autojump
src_files "/usr/share/autojump/autojump.bash"

LBLUE=$'\e[36;40m'
PURPLE=$'\e[35;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'
YELLOW=$'\e[37;40m'
PINK=$'\e[31;40m'

# upstream: https://gist.github.com/lucaslafuga/1766c27ecda6270b9d08
prompt_command() {
	local EXIT="$?"
	history -a

	PS1=""

	case "$TERM" in
	  # https://stackoverflow.com/questions/2068806/gnu-screen-can-you-automatically-name-a-window-after-the-last-invoked-program
	  screen*) PS1='\[\033k\033\\\]'
	esac

	[ $EXIT != 0 ] && PS1+="\[$ORANGE\]$EXIT "
	PS1+='\[$PINK\]\u \[$LBLUE\]at \[$PURPLE\]\h \[$LBLUE\]in '
	PS1+='\[$GREEN\]\w'
	PS1+='\n\[$GREEN\]>> \[$YELLOW\]'
}

PROMPT_COMMAND="prompt_command"

src_files "$HOME/.bashrc_corp"

# vim: ft=sh
