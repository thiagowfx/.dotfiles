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

# environment variables
export SHELL=/usr/bin/zsh
COMPLETION_WAITING_DOTS="true"

# misc
autoload -U zutil
autoload -U compinit && compinit
autoload -U complist
autoload -U colors && colors
setopt appendhistory autocd beep extendedglob nomatch

# prompt 
PROMPT="%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[yellow]%}@%{$reset_color%}%{$fg_bold[blue]%}%m%{$reset_color%}:%{$fg_bold[yellow]%}%~%{$reset_color%} %# "
RPROMPT="%{$fg_bold[blue]%}%? / %*%{$reset_color%}"
# prompt suse

# keybindings
bindkey -e			        # emacs
bindkey '\e[A' history-search-backward
bindkey '\e[B' history-search-forward
# bindkey '^K' kill-whole-line
bindkey "\e[H" beginning-of-line        # Home (xorg)
bindkey "\e[1~" beginning-of-line       # Home (console)
bindkey "\e[4~" end-of-line             # End (console)
bindkey "\e[F" end-of-line              # End (xorg)
bindkey "\e[2~" overwrite-mode          # Ins
bindkey "\e[3~" delete-char             # Delete
bindkey '\eOH' beginning-of-line
bindkey '\eOF' end-of-line

# aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# functions
# ex - archive extractor
ex () {
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

# sort aur packages by their number of votes -- requires cower program
# https://bbs.archlinux.org/viewtopic.php?id=167554
cower_votes() {
    cower --format "%n %o" -s $1 | grep $1 | grep -v extension | sort -nk 2 | column -t
}


function changeroot {
    sudo cp -L /etc/resolv.conf $1/etc/resolv.conf
    sudo mount -t proc proc $1/proc
    sudo mount -t sysfs sys $1/sys
    sudo mount -o bind /dev $1/dev
    sudo mount -t devpts pts $1/dev/pts/
    sudo chroot $1/ /bin/bash
}

# arch linux info
if [[ -f /usr/bin/alsi ]]; then
    alsi    # alt: archey, screenfetch
fi

[[ -f /usr/bin/fortune ]] && /usr/bin/fortune
