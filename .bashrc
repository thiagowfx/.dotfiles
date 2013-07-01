# enable bash autocompletion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# default SHELL
export SHELL=/usr/bin/bash

# add some goodies to PATH
export PATH="/opt/TeamSpeak3-Client-linux_x86:/opt/copy/x86:$PATH"

# default system EDITOR. Other options are: vim, gvim, nano, emacs -nw
export EDITOR="emacs -nw"

# easy updating command
alias upq="sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y dist-upgrade && sudo apt-get -y autoclean && sudo apt-get -y autoremove"
# alias upq="sudo yum -y update && sudo yum -y upgrade" #fedora
# alias upq="sudo zypper -n update && sudo zypper -n upgrade && sudo -n zypper dup" #openSUSE

# force the usage of other programs. Use with caution!

# alias top="htop" # better top
alias vi="vim"
# force opening emacs in terminal (not the X client) while in terminal #alt: install emacs-nox package
alias emacs="emacs -nw" 
alias grep="grep --color=auto"
alias ls="ls -F --color=auto"

# utilities for bash
alias l="ls -al"
alias la="ls -a"
alias ll="ls -l"
alias sai="sudo aptitude install"
alias sar="sudo aptitude remove"
alias sas="sudo aptitude search"
alias mount-iso="mount -o loop"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtuble-dl-video-download="youtube-dl -t"

# windows alias
alias cls="echo Using Unix program clear; clear"
alias tracert="echo Using Unix program traceroute; traceroute"
alias ipconfig="echo Using Unix program ifconfig; ifconfig"
