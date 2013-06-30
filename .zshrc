# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/thiago/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Custom config

# default SHELL
export SHELL=/usr/bin/zsh

# default system editor
export EDITOR="emacs -nw"

# easy updating && cache cleaning command
alias upq="sudo pacman -Syy && sudo pacman -Syu && sudo pacman -Rs $(pacman -Qtdq) --color never"

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
alias mount-iso="mount -o loop"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtuble-dl-video-download="youtube-dl -t"

# windows alias
alias cls="echo Using Unix program clear; clear"
alias tracert="echo Using Unix program traceroute; traceroute"
alias ipconfig="echo Using Unix program ifconfig; ifconfig"

