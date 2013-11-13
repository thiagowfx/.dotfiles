# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# enable bash autocompletion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# enable sudo autocompletion
complete -cf sudo

# config prompt
[[ -f /etc/bashrc ]] && . /etc/bashrc

# without colors
# PS1='[\u@\h \W]\$ '
# with colors
PS1='\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\]\[\033[1;32m\]\$\[\033[m\] '

# tweaks
set -o emacs
shopt -s checkwinsize
shopt -s histappend
# shopt -s cdspell
# shopt -s cmdhist
# shopt -s dotglob
# shopt -s expand_aliases
# shopt -s extglob
# shopt -s hostcomplete
# shopt -s nocaseglob

[[ -f ~/.aliases ]] && . ~/.aliases

alias redo="fc -s"

# Colored man pages
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
