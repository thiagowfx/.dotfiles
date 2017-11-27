 # -*- shell-script -*-

if [[ $- != *i* ]] ; then
	# Shell is non-interactive. Be done now!
	return
fi

# Aliases {{{
src_files() {
	for f in "$@"; do
		[[ -f "$f" ]] && source "$f"
	done
}

src_dirs() {
	for d in "$@"; do
		if [[ -d "$d" ]]; then
			src_files $d/*
		fi
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

add_alias .. "cd .." cd
add_alias ack "ack-grep"
add_alias l "ls -l" ls
add_alias la "ls -al" ls
add_alias sl "ls"
add_alias tmux "tmux -2" tmux
add_alias unstow "stow -D" stow

add_env CLICOLOR "1"
add_env EDITOR "vim" vim && add_env VISUAL "$EDITOR" "$EDITOR"

add_paths "$HOME/.bin"

hash brew &>/dev/null && add_paths "/usr/local/sbin"
# }}}

# Bash history {{{
# Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"
HISTSIZE=50000
HISTFILESIZE="${HISTSIZE}"
HISTIGNORE="ls:la:ll:l:ranger:tree:vdir:history:exit"
# }}}

# Bash shell options {{{
set -o emacs

for option in autocd checkwinsize cdspell cmdhist dotglob expand_aliases extglob histappend hostcomplete nocaseglob; do
	shopt -s $option &>/dev/null
done
# }}}

# Bash colored man pages {{{
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
# }}}

# Bash completion {{{
# Enable completion for sudo.
complete -cf sudo

# Linux bash completion
src_files "/etc/bash_completion" "/usr/share/bash-completion/bash_completion"

# Homebrew bash completion
src_files "/usr/local/etc/bash_completion" "/usr/local/share/bash-completion/bash_completion"
src_dirs  "/usr/local/etc/bash_completion.d"
# }}}

# Command-not-found hooks {{{
hash brew &>/dev/null && brew command command-not-found-init >/dev/null 2>&1 && eval "$(brew command-not-found-init)"
hash pacman &>/dev/null && src_files "/usr/share/doc/pkgfile/command-not-found.bash"
# }}}

# Colors {{{
if command -v tput &>/dev/null && tput setaf 1 &>/dev/null; then
	tput sgr0; # reset colors
	bold=$(tput bold);
	reset=$(tput sgr0);
	# Solarized colors, taken from http://git.io/solarized-colors.
	black=$(tput setaf 0);
	blue=$(tput setaf 33);
	cyan=$(tput setaf 37);
	green=$(tput setaf 64);
	orange=$(tput setaf 166);
	purple=$(tput setaf 125);
	red=$(tput setaf 124);
	violet=$(tput setaf 61);
	white=$(tput setaf 15);
	yellow=$(tput setaf 136);
else
	bold='';
	reset="\e[0m";
	black="\e[1;30m";
	blue="\e[1;34m";
	cyan="\e[1;36m";
	green="\e[1;32m";
	orange="\e[1;33m";
	purple="\e[1;35m";
	red="\e[1;31m";
	violet="\e[1;35m";
	white="\e[1;37m";
	yellow="\e[1;33m";
fi;
# }}}

# Prompts {{{
function prompt_command() {
	local EXIT="$?"
	history -a

	local prompt_symbol="❯"
	local prompt_clean_symbol="☀ "
	local prompt_dirty_symbol="☂ "

	# Git branch name and work tree status (only when we are inside Git working tree)
	if command -v git >/dev/null 2>&1; then
		local git_prompt=
		if [[ "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]]; then
			# Branch name
			local branch
			branch="$(git symbolic-ref HEAD 2>/dev/null)"
			branch="${branch##refs/heads/}"

			# Working tree status (red when dirty)
			local dirty=
			# Modified files
			git diff --no-ext-diff --quiet --exit-code --ignore-submodules 2>/dev/null || dirty=1
			# Untracked files
			[ -z "$dirty" ] && test -n "$(git status --porcelain)" && dirty=1

			# Format Git info
			if [ -n "$dirty" ]; then
				git_prompt=" ${red}$prompt_dirty_symbol$branch${reset}"
			else
				git_prompt=" ${violet}$prompt_clean_symbol$branch${reset}"
			fi
		fi
	fi

	# Set PS1.
	PS1="\[\033]0;\w\007\]"
	PS1+="\[${bold}\]"
	[ $EXIT != 0 ] && PS1+="\[${red}\]$EXIT "
	PS1+="\[${orange}\]\u" # username
	PS1+="\[${white}\] at " # at
	PS1+="\[${yellow}\]\h" # host
	PS1+="\[${white}\] in " # in
	PS1+="\[${green}\]\w" # working directory
	command -v git >/dev/null 2>&1 && PS1+=$git_prompt # git
	PS1+="\n";
	PS1+="\[${white}\]$prompt_symbol \[${reset}\]"; # `$` (and reset color)
	# Set PS2.
	PS2="\[${yellow}\]→ \[${reset}\]";
}

# http://askubuntu.com/questions/80371/bash-history-handling-with-multiple-terminals
PROMPT_COMMAND="prompt_command"
# }}}

# vim: fdm=marker ft=sh
