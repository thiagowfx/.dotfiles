## user aliases

# easy updating command
alias upq="sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y dist-upgrade && sudo apt-get -y autoclean && sudo apt-get -y autoremove"
# Fedora
# alias upq="sudo yum -y update && sudo yum -y upgrade"
# openSUSE
# alias upq="sudo zypper -n update && sudo zypper -n upgrade && sudo -n zypper dup"

# force opening emacs in terminal (not the X client) while in terminal
alias emacs="emacs -nw"

# utilities for bash
alias l="ls -al"
alias la="ls -a"
alias ll="ls -l"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtuble-dl-video-download="youtube-dl -t"
