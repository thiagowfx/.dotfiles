# -*- shell-script -*-

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

# Set history.
# Ignore space and ignore duplicates.
HISTCONTROL="ignoreboth"
HISTSIZE=100000
HISTFILESIZE="${HISTSIZE}"

# Enable completion for sudo.
complete -cf sudo

# Set shell options.
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

# HELPER: source the given file if it exists.
sofe() {
	[[ -f "$@" ]] && source "$@"
}

# HELPER: source the given directory if it exists.
sofd() {
	if [[ -d "$@" ]]; then
		for f in "$@"/*; do
			source "$f"
		done
	fi
}

# Fix R-lang start-up.
export LANG=${LANG:-en_US.UTF-8}

# Define a few colors.
_set_colors() {
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
}
_set_colors

# Return the git prompt.
_prompt_git() {
	local s='';
	local branchName='';

	# Check if the current directory is in a Git repository.
	if [ $(git rev-parse --is-inside-work-tree &>/dev/null; echo "${?}") == '0' ]; then
		# check if the current directory is in .git before running git checks
		if [ "$(git rev-parse --is-inside-git-dir 2> /dev/null)" == 'false' ]; then

			# Ensure the index is up to date.
			git update-index --really-refresh -q &>/dev/null;

			# Check for uncommitted changes in the index.
			if ! $(git diff --quiet --ignore-submodules --cached); then
				s+='+';
			fi;

			# Check for unstaged changes.
			if ! $(git diff-files --quiet --ignore-submodules --); then
				s+='!';
			fi;

			# Check for untracked files.
			if [ -n "$(git ls-files --others --exclude-standard)" ]; then
				s+='?';
			fi;

			# Check for stashed files.
			if $(git rev-parse --verify refs/stash &>/dev/null); then
				s+='$';
			fi;

		fi;

		# Get the short symbolic ref.
		# If HEAD isn’t a symbolic ref, get the short SHA for the latest commit
		# Otherwise, just give up.
		branchName="$(git symbolic-ref --quiet --short HEAD 2> /dev/null || \
			git rev-parse --short HEAD 2> /dev/null || \
			echo '(unknown)')";

		[ -n "${s}" ] && s=" [${s}]";

		echo -e "${1}${branchName}${yellow}${s}";
	else
		return;
	fi;
}

# Set both prompts.
PS1="\[\033]0;\w\007\]"
PS1+="\[${bold}\]"
PS1+="\[${orange}\]\u" # username
PS1+="\[${white}\] at "
PS1+="\[${yellow}\]\h" # host
PS1+="\[${white}\] in "
PS1+="\[${green}\]\w" # working directory
PS1+="\$(_prompt_git \"${white} on ${violet}\")"; # Git repository details
PS1+="\n";
PS1+="\[${white}\]\$ \[${reset}\]"; # `$` (and reset color)
PS2="\[${yellow}\]→ \[${reset}\]";

# Add colors to man pages.
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

# Source utilities.
# DEPENDS: sofd, sofe
_source_backpack() {
	# Bash completion.
	sofe "/etc/bash_completion"
	sofe "/opt/local/etc/profile.d/bash_completion.sh"
	sofd "/usr/local/etc/bash_completion.d"

	# Command-not-found hooks.
	sofe "/usr/share/doc/pkgfile/command-not-found.bash"

	# Autojump (j).
	sofe "/etc/profile.d/autojump.bash"
	sofe "/opt/local/etc/profile.d/autojump.sh"
	sofe "/usr/share/autojump/autojump.bash"
	command -v brew &>/dev/null && sofe "$(brew --prefix)/etc/profile.d/autojump.sh"

	# Termite.
	[[ $TERM == xterm-termite ]] && sofe "/etc/profile.d/vte.sh"
}
_source_backpack

# Creates an alias iff the specified program and/or the given file
# exists on the system.
#
# Arguments:
#  $1 (mandatory): the alias
#  $2 (mandatory): its value
#  $3: a program (e.g. vim or /usr/bin/vim)
#  $4: a file (e.g. $HOME/directory)
#
addalias() {
	[[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
	alias "$1"="$2"
}

# Sets an environment variable iff its correlated program
# is installed and/or if the given file exists on the system.
#
# Arguments:
#  $1 (mandatory): the environment variable (e.g. EDITOR)
#  $2 (mandatory): its value
#  $3: a program (e.g. vim or /usr/bin/vim)
#  $4: a file (e.g. $HOME/directory)
#
addenv() {
	[[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
	[[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
	export "$1"="$2"
}

# Add the given argument to PATH.
addpath() {
	addenv PATH "$1:$PATH"
}

# Source more utilities.
# DEPENDS: addalias, addenv
_source_backpack_2() {
	addalias make "make -j"
	addalias tmux "tmux -2" tmux
	addalias xclip "xclip -selection clipboard" xclip

	addalias cower "cower --color=always --sort=votes" cower

	addalias chmod "chmod -v" chmod
	addalias chown "chown -v" chown
	addalias cp "cp -v" cp
	addalias curl "curl -v -L" curl
	addalias df "df -h" df
	addalias du "du -h" du
	addalias free "free -h" free
	addalias grep "grep --color=always" grep
	addalias hexdump "hexdump -C" hexdump
	addalias ln "ln -v" ln
	addalias mv "mv -v" mv
	addalias netstat "netstat -pln" netstat
	addalias pgrep "pgrep -fl" pgrep
	addalias pstree "pstree -p" pstree

	addalias ls "ls -F" ls
	addalias sl "ls" ls
	addalias l "ls -l" ls
	addalias ll "l" ls

	addalias g "git" git

	addalias diff "diff -uN" diff
	addalias diff "colordiff -uN" colordiff
	addalias colordiff "colordiff -uN" colordiff

	addalias youtube-dl-mp3 "youtube-dl --continue --title --restrict-filenames --extract-audio --audio-format mp3"
	addalias youtube-dl-video "youtube-dl --continue --title --restrict-filenames"

	addenv EDITOR "vim" vim
	addenv VISUAL "$EDITOR"
	addenv LESS "-R" less
	addenv GTEST_COLOR "YES"

	addpath "$HOME/bin"
	addpath "$HOME/.bin"

	addpath "/opt/local/bin"
	addpath "/opt/local/sbin"
	addpath "/usr/local/sbin"

	command -v ruby &>/dev/null && addpath "$(ruby -rubygems -e "puts Gem.user_dir")/bin"

	addpath "/usr/lib/ccache/bin"
}
_source_backpack_2