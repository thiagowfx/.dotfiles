# -*- shell-script -*-

# Interactive
# if not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# Source aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# homebrew: command-not-found
src_file "/usr/local/Homebrew/Library/Taps/homebrew/homebrew-command-not-found/handler.sh"

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
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# oh-my-zsh
zplug "lib/history", from:oh-my-zsh
zplug "plugins/docker", from:oh-my-zsh

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"

zplug "rupa/z", use:"z.sh"
zplug "sindresorhus/pure", use:"{pure,async}.zsh"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
	zplug install
fi

# Then, source plugins and add commands to $PATH
zplug load --verbose
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

# vim: fdm=marker
