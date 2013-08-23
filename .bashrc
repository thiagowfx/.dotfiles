# prompt
PS1='[\u@\h \W]\$ '

# enable bash autocompletion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# enable sudo autocompletion
complete -cf sudo

set -o emacs
# shopt -s cdspell
# shopt -s checkwinsize
# shopt -s cmdhist
# shopt -s dotglob
# shopt -s expand_aliases
# shopt -s extglob
# shopt -s histappend
# shopt -s hostcomplete
# shopt -s nocaseglob

export SHELL=/usr/bin/bash

# aliases
[[ -f ~/dotfiles/aliases ]] && . ~/dotfiles/aliases

# bash-only aliases
alias redo="fc -s"
