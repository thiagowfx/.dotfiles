#!/bin/bash
# shellcheck disable=SC2034
# Set bash prompt.

# Monokai-inspired: https://gist.github.com/transat/6694554
RESET=$'\e[0m'
BOLD=$'\e[1m'
RED=$'\e[31;40m'
BLUE=$'\e[34;40m'
LIGHTBLUE=$'\e[36;40m'
GREEN=$'\e[32;40m'
ORANGE=$'\e[33;40m'
PINK=$'\e[31;40m'
PURPLE=$'\e[35;40m'
YELLOW=$'\e[37;40m'

prompt_command() {
	# this must be the first line
	local EXIT="$?"

	# append command to history
	history -a

	# start with blank prompt
	PS1="\\[$RESET\\]"

	# add exit code from previous command if unsuccessful
	[ $EXIT != 0 ] && PS1+="\\[$RED\\]$EXIT "

	# add user, hostname and directory
	PS1+='\[$BLUE\]\u\[$RESET\]@\h \[$BOLD\]\w\[$RESET\]'

	# add vanilla git prompt if existing
	hash __git_ps1 &>/dev/null && PS1+="\\[$GREEN\\]\$(__git_ps1)"

	# add prompt
	PS1+='\n\[$ORANGE\]>> \[$RESET\]'

	# support OSC7 for VTE-based terminals if existing
	hash __vte_osc7 &>/dev/null && __vte_osc7
}

PROMPT_COMMAND="prompt_command"
