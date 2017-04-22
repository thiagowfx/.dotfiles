 # -*- shell-script -*-

# Interactive {{{
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen!
# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive. Be done now!
	return
fi
# }}}

# Source aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# Bash History {{{
# Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"
HISTSIZE=100000
HISTFILESIZE="${HISTSIZE}"

# History: work with multiple sessions
# Upstream: http://askubuntu.com/questions/80371/bash-history-handling-with-multiple-terminals
export PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
# }}}

# Shell options {{{
set -o emacs

shopt -s autocd
shopt -s checkwinsize
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s expand_aliases
shopt -s extglob
shopt -s histappend
shopt -s hostcomplete
shopt -s nocaseglob
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

src_file "/etc/bash_completion"
src_dir "~/.bash_completion.d"

# MacPorts bash completion
src_file "/opt/local/etc/profile.d/bash_completion.sh"
src_dir "/opt/local/share/bash-completion/completions"

# HomeBrew bash completion
src_file "/usr/local/etc/bash_completion"
src_dir  "/usr/local/etc/bash_completion.d"
src_file "/usr/local/share/bash-completion/bash_completion"
# }}}

# Command-not-found hooks {{{
# Pkgfile (for pacman)
src_file "/usr/share/doc/pkgfile/command-not-found.bash"

# HomeBrew
if command -v brew &>/dev/null && brew command command-not-found-init >/dev/null 2>&1; then
	eval "$(brew command-not-found-init)"
fi
# }}}

# Autojump (j) {{{
src_file "/etc/profile.d/autojump.bash"
src_file "/usr/share/autojump/autojump.bash"

# MacPorts
src_file "/opt/local/etc/profile.d/autojump.sh"

# HomeBrew
src_file "/usr/local/etc/profile.d/autojump.sh"
# }}}

# Prompts {{{
# Git prompt.
src_file "$HOME/.git-prompt.sh"

# Set colors.
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

# Set PS1.
PS1="\[\033]0;\w\007\]"
PS1+="\[${bold}\]"
PS1+="\[${orange}\]\u" # username
PS1+="\[${white}\] at "
PS1+="\[${yellow}\]\h" # host
PS1+="\[${white}\] in "
PS1+="\[${green}\]\w" # working directory
PS1+="${violet}\$(__git_ps1)"
PS1+="\n";
PS1+="\[${white}\]\$ \[${reset}\]"; # `$` (and reset color)

# Set PS2.
PS2="\[${yellow}\]→ \[${reset}\]";
# }}}



# vim: fdm=marker ft=sh
