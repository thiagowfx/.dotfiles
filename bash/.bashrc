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

# History {{{
# Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"
HISTSIZE=100000
HISTFILESIZE="${HISTSIZE}"

# History: work with multiple sessions
# Upstream: http://askubuntu.com/questions/80371/bash-history-handling-with-multiple-terminals
export PROMPT_COMMAND="history -a ; $PROMPT_COMMAND"
# }}}

# Shell options {{{
set -o emacs
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

# Helper functions {{{
# HELPER: source the given file(s) if it exists.
src_file() {
	for f in "$@"; do
		[[ -f "$f" ]] && source "$f"
	done
}

# HELPER: source the given directory(ies) if it exists.
src_dir() {
	for d in "$@"; do
		if [[ -d "$d" ]]; then
			src_file $d/*
		fi
	done
}

# HELPER: Creates an alias iff the specified program and/or the given file
# exists on the system.
#
# Arguments:
#  $1 (mandatory): the alias
#  $2 (mandatory): its value
#  $3 (optional): a program (e.g. vim or /usr/bin/vim)
#  $4 (optional): a file (e.g. $HOME/directory)
#
add_alias() {
	[[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
	alias "$1"="$2"
}

# HELPER: Sets an environment variable iff its correlated program
# is installed and/or if the given file exists on the system.
#
# Arguments:
#  $1 (mandatory): the environment variable (e.g. EDITOR)
#  $2 (mandatory): its value
#  $3 (optional): a program (e.g. vim or /usr/bin/vim)
#  $4 (optional): a file (e.g. $HOME/directory)
#
add_env() {
	[[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
	export "$1"="$2"
}

# HELPER: Prepend the given argument(s) to the PATH variable.
add_path() {
	for d in "$@"; do
		add_env PATH "$d:$PATH" "" "$d"
	done
}
# }}}

# Colored man pages {{{
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
src_file "~/.bash_completion.d"

# MacPorts bash completion
src_file "/opt/local/etc/profile.d/bash_completion.sh"
src_dir "/opt/local/share/bash-completion/completions"

# HomeBrew bash completion
src_dir "/usr/local/etc/bash_completion.d"
# }}}

# thefuck {{{
command -v thefuck &>/dev/null && eval "$(thefuck --alias)"
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

# change command behavior {{{
if command -v nproc &>/dev/null; then
	NPROC=$(nproc)
elif command -v sysctl &>/dev/null; then
	NPROC=$(sysctl -n hw.ncpu)
else
	NPROC=${NPROC:-1}
fi

add_alias make "make -j${NPROC} -l${NPROC}" make
add_alias xclip "xclip -selection clipboard" xclip
# }}}

# cd {{{
add_alias .. "cd .." cd
add_alias ... "cd ..." cd
# }}}

# verbosity / human-friendliness {{{
add_alias chmod "chmod -v" chmod
add_alias chown "chown -v" chown
add_alias cp "cp -v" cp

add_alias cower "cower --color=always --sort=votes" cower
add_alias curl "curl -v -L" curl
add_alias du "du -h" du
add_alias free "free -h" free
add_alias grep "grep --color=always" grep
add_alias hexdump "hexdump -C" hexdump
add_alias ln "ln -v" ln
add_alias mv "mv -v" mv
add_alias netstat "netstat -pln" netstat
add_alias pgrep "pgrep -fl" pgrep
add_alias pstree "pstree -p" pstree
# }}}

# {{{ ls
add_alias ls "ls -F" ls
add_alias sl "ls" ls
add_alias l "ls -l -F" ls
add_alias ll "l" ls
# }}}

# command abbreviation / alternatives {{{
add_alias ack "ack-grep" ack
add_alias cmakee "cmake --warn-uninitialized --warn-unused-vars --check-system-vars -Wno-dev" cmake
add_alias g "git" git
add_alias i3lock "i3lock -c 777777" i3lock
add_alias pingg "ping google.com" ping
add_alias tmux "tmux -2" tmux
add_alias xclip "xclip -selection clipboard" xclip
add_alias unstow "stow -D" stow

t-cmake-clean() {
       local BUILD=$(basename $(pwd))
       cd ..
       rm -rf $BUILD
       mkdir $BUILD && cd $BUILD
}
# }}}

# diff {{{
add_alias colordiff "colordiff -uN" colordiff
add_alias diff "diff -uN" diff
add_alias diff "colordiff -uN" colordiff
# }}}

# youtube-dl {{{
add_alias youtube-dl-mp3   "youtube-dl --continue --title --restrict-filenames --extract-audio --audio-format mp3" youtube-dl
add_alias youtube-dl-video "youtube-dl --continue --title --restrict-filenames" youtube-dl
# }}}

# Unix variables {{{
add_env EDITOR "vim" vim
add_env VISUAL "$EDITOR" "$EDITOR"
add_env LESS "-R" less
# }}}

# environment variables {{{
add_env GTEST_COLOR "YES"
# }}}

# PATH {{{
add_path "$HOME/bin" "$HOME/.bin"

# MacPorts
add_path "/opt/local/bin" "/opt/local/sbin"

# HomeBrew
add_path "/usr/local/sbin"

# RubyGems
command -v ruby &>/dev/null && add_path "$(ruby -rubygems -e "puts Gem.user_dir")/bin"
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

# python: virtualenvwrapper {{{
# HomeBrew
add_env WORKON_HOME "$HOME/.virtualenvs" virtualenvwrapper.sh "/usr/local/bin/virtualenvwrapper.sh"
src_file "/usr/local/bin/virtualenvwrapper.sh"
# }}}

# vim: fdm=marker
