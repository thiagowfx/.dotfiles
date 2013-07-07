# prompt
PS1='[\u@\h \W]\$ '

# enable bash autocompletion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# enable sudo autocompletion
complete -cf sudo

# xhost +local:root > /dev/null 2>&1

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
# add some goodies to PATH
export PATH="/opt:$PATH"
export EDITOR="emacs -nw"
export BROWSER=/usr/bin/xdg-open

# easy updating command (Ubuntu version)
alias upq="sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y dist-upgrade && sudo apt-get -y autoclean && sudo apt-get -y autoremove"
# alias upq="sudo yum -y update && sudo yum -y upgrade" #fedora
# alias upq="sudo zypper -n update && sudo zypper -n upgrade && sudo -n zypper dup" #openSUSE

alias vi="vim"
alias grep="grep --color=auto"
alias ls="ls -F --color=auto"
# force opening emacs in terminal (not the X client) while in terminal #alt: install emacs-nox package
alias emacs="emacs -nw" 
alias l="ls -al"
alias sai="sudo aptitude install"
alias sar="sudo aptitude remove"
alias sas="sudo aptitude search"
alias mount-iso="mount -o loop"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtuble-dl-video-download="youtube-dl -t"
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

# windows alias
alias cls="echo Using Unix program clear; clear"
alias tracert="echo Using Unix program traceroute; traceroute"
alias ipconfig="echo Using Unix program ifconfig; ifconfig"

# ex - archive extractor
# usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

