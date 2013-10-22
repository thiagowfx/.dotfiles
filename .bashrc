# enable bash autocompletion
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# enable sudo autocompletion
complete -cf sudo

# config prompt
[[ -f /etc/bashrc ]] && . /etc/bashrc
# PS1='[\u@\h \W]\$ ' # without colors
PS1='\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\]\[\033[1;32m\]\$\[\033[m\] ' # with colors

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

# aliases
[[ -f ~/.aliases ]] && . ~/.aliases

alias redo="fc -s"
