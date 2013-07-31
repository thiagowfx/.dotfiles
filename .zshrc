#########################################################################
# Copyright (C) 2013-2013 Thiago Barroso Perrotta.                      #
#                                                                       #
# This program is free software: you can redistribute it and/or modify  #
# it under the terms of the GNU General Public License as published by  #
# the Free Software Foundation, either version 3 of the License, or     #
# (at your option) any later version.                                   #
#                                                                       #
# This program is distributed in the hope that it will be useful,       #
# but WITHOUT ANY WARRANTY; without even the implied warranty of        #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
# GNU General Public License for more details.                          #
#                                                                       #
# You should have received a copy of the GNU General Public License     #
# along with this program.  If not, see <http://www.gnu.org/licenses/>. #
#########################################################################

# variables
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
COMPLETION_WAITING_DOTS="true"
RPROMPT=""
export SHELL=/usr/bin/zsh
export PATH="/opt:$PATH"
export EDITOR="emacs -nw"
export VISUAL="emacs"
export BROWSER=/usr/bin/xdg-open

# misc
autoload -U zutil
autoload -U compinit
compinit
autoload -U complist
setopt appendhistory autocd beep extendedglob nomatch 
prompt suse

# keybindings
bindkey -e			        # emacs
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
bindkey '^K' kill-whole-line
bindkey "\e[H" beginning-of-line        # Home (xorg)
bindkey "\e[1~" beginning-of-line       # Home (console)
bindkey "\e[4~" end-of-line             # End (console)
bindkey "\e[F" end-of-line              # End (xorg)
bindkey "\e[2~" overwrite-mode          # Ins
bindkey "\e[3~" delete-char             # Delete
bindkey '\eOH' beginning-of-line
bindkey '\eOF' end-of-line

# distro specific commands
if [[ -f /etc/arch-release  ]]; then
    alias world="sudo pacman -Syy && sudo pacman -Syu && sudo pacman-optimize"
elif
    [[ -f /etc/manjaro-release ]]; then
    alias world="sudo pacman -Syy && sudo pacman -Syu && sudo pacman-optimize"
elif
    [[ -f /etc/debian-release ]]; then
    alias world="sudo apt-get -y update && sudo apt-get -y upgrade && sudo apt-get -y dist-upgrade && sudo apt-get -y autoclean && sudo apt-get -y autoremove" # alt: aptitude
    # fedora
    # alias world="sudo yum -y update && sudo yum -y upgrade"
    # openSUSE
    # alias world="sudo zypper -n update && sudo zypper -n upgrade && sudo -n zypper dup"
fi

# aliases, tweaking some common tasks
alias cp="cp -v -i"                         # confirm before overwriting something
alias chmod="chmod -v"
alias chown="chown -v"
alias df='df -h'                          # human-readable sizes
alias cp="cp -i -v"
alias em="emacs -nw"
alias free='free -m'                      # show sizes in MB
alias grep="grep --color=auto -E -d skip"
alias l="ls -1A --color=auto"
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ln="ln -v"
alias mv="mv -v"
alias pgrep="pgrep --list-name"
alias rm="rm -v -i"
alias vi="vim"

# shorthands

# mount cd/dvd
alias mount-iso="mount -o loop"

# require youtube-dl package
alias youtube-dl-mp3-download="youtube-dl -t --restrict-filenames --extract-audio --audio-format mp3"
alias youtube-dl-video-download="youtube-dl -t --restrict-filenames"

# most used commands - based on http://crunchbang.org/forums/viewtopic.php?id=6487
alias top10="fc -l 0 | awk '{print \$2}' | awk 'BEGIN {FS=\"|\"} {print \$1}' | sort | uniq -c | sort -rn | head -n 10"

# windows
alias cls="clear"
alias tracert="traceroute"
alias ipconfig="ifconfig"	# alt: ip addr

# functions
# ex - archive extractor
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
function changeroot {
    sudo cp -L /etc/resolv.conf $1/etc/resolv.conf
    sudo mount -t proc proc $1/proc
    sudo mount -t sysfs sys $1/sys
    sudo mount -o bind /dev $1/dev
    sudo mount -t devpts pts $1/dev/pts/
    sudo chroot $1/ /bin/bash
}

# output
if [[ -f /usr/bin/alsi ]]; then
    alsi    # alt: archey, screenfetch
fi


if [[ -e ~/.zshrc-custom ]]; then
    source ~/.zshrc-custom
fi
