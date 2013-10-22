# misc
autoload -U zutil
autoload -U compinit && compinit
autoload -U complist
autoload -U promptinit && promptinit
autoload -U colors && colors
setopt appendhistory autocd beep extendedglob nomatch hist_ignore_space hist_ignore_all_dups

# enable zsh syntax highlightning
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh ]] && source /usr/share/zsh/plugins/zsh-syntax-highlight/zsh-syntax-highlighting.zsh
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# my old zsh prompt 
# PROMPT="%(?,%{$bg[green]%}✔,%{$bg[red]%}✘)%{$reset_color%}%{$fg_bold[blue]%}%n%{$reset_color%}%{$fg_bold[yellow]%}@%{$reset_color%}%{$fg_bold[blue]%}%m%{$reset_color%}:%{$fg_bold[yellow]%}%~%{$reset_color%} %# "
# RPROMPT="%{$fg_bold[blue]%}%? / %*%{$reset_color%}"
