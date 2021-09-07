#!/bin/bash
# shellcheck disable=SC2034
# Set bash prompt.

# Monokai-inspired: https://gist.github.com/transat/6694554
RESET=$'\e[0m'
BOLD=$'\e[1m'
RED=$'\e[31;49m'
BLUE=$'\e[34;49m'
LIGHTBLUE=$'\e[36;49m'
GREEN=$'\e[32;49m'
ORANGE=$'\e[33;49m'
PINK=$'\e[31;49m'
PURPLE=$'\e[35;49m'
YELLOW=$'\e[37;49m'

# git prompt customization: https://wiki.archlinux.org/title/Git
if hash git >/dev/null 2>&1; then
	GIT_PS1_SHOWDIRTYSTATE=1

	# arch
	src_files "/usr/share/git/git-prompt.sh"

	# debian: https://packages.debian.org/sid/amd64/git/filelist
	src_files "/usr/lib/git-core/git-sh-prompt"
fi

prompt_command() {
	# this must be the first line
	local EXIT="$?"

	# append command to history
	history -a

	# start with blank prompt
	PS1="\\[$RESET\\]"

	# add exit code from previous command if unsuccessful
	[ $EXIT != 0 ] && PS1+="\\[$BOLD\\]\\[$RED\\]$EXIT "

	# add user, hostname and directory
	PS1+='\[$BOLD\]\[$BLUE\]\u\[$RESET\]@\h \[$BOLD\]\w\[$RESET\]'

	# add vanilla git prompt if existing
	hash __git_ps1 &>/dev/null && PS1+="\\[$GREEN\\]\$(__git_ps1)"

	# add prompt
	PS1+='\n\[$ORANGE\]>> \[$RESET\]'

	# support OSC7 for VTE-based terminals if existing
	hash __vte_osc7 &>/dev/null && __vte_osc7
}
PROMPT_COMMAND="prompt_command"
