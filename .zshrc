HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt beep
bindkey -e
# zstyle :compinstall filename '/home/thiago/.zshrc'

fpath=($HOME/.zsh/functions $fpath)
 
# colors
# eval `dircolors $HOME/.zsh/colors`
 
autoload -U zutil
autoload -U compinit
autoload -U complist
 
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
 
# Activation
compinit
 
# Resource files
# for file in $HOME/.zsh/rc/*.rc; do
#         source $file
# done

prompt suse
RPROMPT=""

# easy updating && cache cleaning command
alias upq="sudo pacman -Syy && sudo pacman -Syu" # && sudo pacman -Rs $(pacman -Qtdq) --color never"
export PATH="/opt:$PATH"
export EDITOR="emacs -nw"
export BROWSER=/usr/bin/xdg-open
export SHELL=/usr/bin/bash
export PATH="/opt:$PATH"
export EDITOR="emacs -nw"
export BROWSER=/usr/bin/xdg-open
alias vi="vim"
alias grep="grep --color=auto"
alias ls="ls -F --color=auto"
alias l="ls -al"
alias sai="sudo aptitude install"
alias sar="sudo aptitude remove"
alias sas="sudo aptitude search"
alias mount-iso="mount -o loop"
alias youtube-dl-mp3-download="youtube-dl -t --extract-audio --audio-format mp3"
alias youtube-dl-video-download="youtube-dl -t"
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB
alias cls="echo Using Unix program clear; clear"
alias tracert="echo Using Unix program traceroute; traceroute"
alias ipconfig="echo Using Unix program ifconfig; ifconfig"
#alias top10="fc -l 0 |awk '{print $2}'|awk 'BEGIN {FS="|"} {print $1}'|sort|uniq -c|sort -rn|head -10"

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

alsi
