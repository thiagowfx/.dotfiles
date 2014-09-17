# -*- sh -*-

[[ -f ~/.aliases ]] && . ~/.aliases

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# autoloads
autoload -U bashcompinit && bashcompinit
autoload -U colors && colors
autoload -U compinit && compinit
autoload -U complist
autoload -U promptinit && promptinit
autoload -U zutil

zstyle ':completion:*' rehash true
zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

[[ -f /usr/share/doc/pkgfile/command-not-found.zsh ]] && . /usr/share/doc/pkgfile/command-not-found.zsh
[[ -f /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && . /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

# options
prompt suse
setopt appendhistory
setopt autocd
setopt beep
setopt emacs
setopt extendedglob
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt nomatch
setopt printexitvalue
