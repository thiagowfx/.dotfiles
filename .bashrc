# enable bash autocompletion feature
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

## user aliases

# add to PATH
export PATH="/opt/TeamSpeak3-Client-linux_x86/:$PATH"

# default EDITOR. Other options: vim, gvim, nano
export EDITOR="emacs -nw"

# easy updating command
alias upq="sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y dist-upgrade && sudo apt-get -y autoclean && sudo apt-get -y autoremove"
# Fedora
# alias upq="sudo yum -y update && sudo yum -y upgrade"
# openSUSE
# alias upq="sudo zypper -n update && sudo zypper -n upgrade && sudo -n zypper dup"

# force the usage of other programs. Use with caution!
alias top="htop"
alias vi="vim"
alias emacs="emacs -nw" # force opening emacs in terminal (not the X client) while in terminal

# utilities for bash
alias l="ls -al"
alias la="ls -a"
alias ll="ls -l"
alias sai="sudo aptitude install"
alias sar="sudo aptitude remove"
alias sash="sudo aptitude show"
alias sase="sudo aptitude search"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtuble-dl-video-download="youtube-dl -t"

# windows alias
alias cls="echo Using Unix program clear; clear"
alias tracert="echo Using Unix program traceroute; traceroute"
alias ipconfig="echo Using Unix program ifconfig; ifconfig"
