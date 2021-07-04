#!/bin/bash
# Set bash prompt.

BLUE=$'\e[34;49m'
GREEN=$'\e[32;49m'
ORANGE=$'\e[33;49m'
PURPLE=$'\e[35;49m'
RED=$'\e[31;49m'
RESET=$'\e[0m'

# https://gist.github.com/lucaslafuga/1766c27ecda6270b9d08
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
	PS1+='\[$GREEN\]\u@\h \[$BLUE\]\w'

	# add upstream git prompt if existing
	hash __git_ps1 &>/dev/null && PS1+="\\[$PURPLE\\]\$(__git_ps1)"

	# add prompt
	PS1+='\n\[$ORANGE\]>> \[$RESET\]'

	# support OSC7 for VTE-based terminals if existing
	hash __vte_osc7 &>/dev/null && __vte_osc7
}

PROMPT_COMMAND="prompt_command"
