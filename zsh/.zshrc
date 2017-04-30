# -*- shell-script -*-

# Interactive
# if not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# Source aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# Shell options {{{
setopt autocd
setopt auto_list
setopt autopushd
setopt beep
setopt correctall
setopt emacs
setopt extendedglob
setopt nohashdirs
setopt nomatch
setopt notify
setopt printexitvalue
setopt prompt_subst

# rehash new executables
zstyle ':completion:*' rehash true

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2
# }}}

# Plug-ins {{{
src_file "$HOME/.zplug/init.zsh"

# let zplug manage itself
# Comment it out for now, as it breaks commands. See https://github.com/zplug/zplug/pull/372
# zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# prompt
zplug "sindresorhus/pure", use:"{pure,async}.zsh"

# core
zplug "zsh-users/zaw" # C-x ;
zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting" && zplug "zsh-users/zsh-history-substring-search"

# oh-my-zsh
zplug "lib/history", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh

# plug-ins
zplug "rupa/z", use:"z.sh"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
	zplug install
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose
# }}}

# Keyboard shortcuts {{{
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
# }}}

# Autoloads {{{
autoload -U bashcompinit && bashcompinit
autoload -U colors && colors
autoload -U compinit && compinit
autoload -U complist
autoload -Uz promptinit && promptinit
autoload -Uz vcs_info
autoload -U zutil
# }}}

# vim: fdm=marker ft=zplug
