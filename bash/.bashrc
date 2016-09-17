# -*- shell-script -*-

# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !
# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
  # Shell is non-interactive. Be done now!
  return
fi

source_if_exists() {
    [[ -f "$@" ]] && source "$@"
}

source_if_exists "$HOME/.ishells.sh"

source_if_exists "/usr/share/autojump/autojump.bash"
source_if_exists "/etc/profile.d/autojump.bash"
source_if_exists "/opt/local/etc/profile.d/autojump.sh"

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

HISTCONTROL="ignoreboth" # ignorespace and ignoredups
HISTSIZE=100000
HISTFILESIZE="$HISTSIZE"

source_if_exists "/etc/bashrc"
source_if_exists "/etc/bash_completion"
source_if_exists "/opt/local/etc/profile.d/bash_completion.sh"
complete -cf sudo
source_if_exists "/usr/share/doc/pkgfile/command-not-found.bash"

if [[ -d "/usr/local/etc/bash_completion.d/" ]]; then
	for f in /usr/local/etc/bash_completion.d/*; do
		source $f
	done
fi

source_if_exists "$HOME/.bash_prompt"

# colored man pages
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
