# -*- shell-script -*-

# Interactive
# if not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# Source aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# zsh: commands beginning with blank spaces don't go to the history
export HIST_IGNORE_SPACE

# Shell options {{{
setopt appendhistory
setopt beep
setopt emacs
setopt extendedglob
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt nohashdirs
setopt nomatch
setopt printexitvalue
setopt prompt_subst
# }}}

# rehash
zstyle ':completion:*' rehash true

# show completion menu when number of options is at least 2
zstyle ':completion:*' menu select=2

# Plug-ins {{{
src_file "$HOME/.zplug/init.zsh"

zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting"

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
