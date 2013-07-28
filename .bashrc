# prompt
PS1='[\u@\h \W]\$ '

# enable bash autocompletion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# enable sudo autocompletion
complete -cf sudo

# shopt -s cdspell
# shopt -s checkwinsize
# shopt -s cmdhist
# shopt -s dotglob
# shopt -s expand_aliases
# shopt -s extglob
# shopt -s histappend
# shopt -s hostcomplete
# shopt -s nocaseglob

export HISTSIZE=10000
export HISTFILESIZE=${HISTSIZE}
export HISTCONTROL=ignoreboth
export SHELL=/usr/bin/bash
export PATH="/opt:$PATH"
export EDITOR="emacs -nw"
export BROWSER=/usr/bin/xdg-open

# bash-only aliases
alias redo="fc -s"
