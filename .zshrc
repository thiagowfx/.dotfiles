# environment variables
export SHELL=/usr/bin/zsh
COMPLETION_WAITING_DOTS="true"

# misc
autoload -U zutil
autoload -U compinit && compinit
autoload -U complist
autoload -U promptinit && promptinit
autoload -U colors && colors
setopt appendhistory autocd beep extendedglob nomatch hist_ignore_space hist_ignore_all_dups

# prompt 
PROMPT="%(?,%{$bg[green]%}✔,%{$bg[red]%}✘)%{$reset_color%}%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[yellow]%}@%{$reset_color%}%{$fg_bold[blue]%}%m%{$reset_color%}:%{$fg_bold[yellow]%}%~%{$reset_color%} %# "
RPROMPT="%{$fg_bold[blue]%}%? / %*%{$reset_color%}"
# prompt suse
zstyle ':completion:*:descriptions' format '%U%B%d%b%u' 
zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'

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

[[ -f /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh ]] && source /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# [[ -f /usr/bin/fortune ]] && /usr/bin/fortune

# zsh-only aliases
# most used commands @ ZSH - based on http://crunchbang.org/forums/viewtopic.php?id=6487
alias zsh-stats="fc -l 0 | awk '{print \$2}' | awk 'BEGIN {FS=\"|\"} {print \$1}' | sort | uniq -c | sort -rn | head -n 10"
