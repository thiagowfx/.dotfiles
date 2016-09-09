# -*- shell-script -*-

# if not running interactively, don't do anything
[[ -z "$PS1" ]] && return

source_if_exists() {
    [[ -f "$@" ]] && source "$@"
}

source_if_exists "$HOME/.ishells.sh"

source_if_exists "/usr/share/autojump/autojump.zsh"
source_if_exists "/etc/profile.d/autojump.zsh"
source_if_exists "/opt/local/etc/profile.d/autojump.sh"

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

# zsh: commands beginning with blank spaces don't go to the history
export HIST_IGNORE_SPACE

autoload -U bashcompinit && bashcompinit
autoload -U colors && colors
autoload -U compinit && compinit
autoload -U complist
autoload -Uz promptinit && promptinit
autoload -Uz vcs_info
autoload -U zutil

# VCS information and PS1 prompt
# upstream: http://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:git*' formats "(%{$fg[grey]%}%s)-[%{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%}]"
precmd() { vcs_info }
PS1='${vcs_info_msg_0_}%# '
PS1="%n@%m:%~/ $PS1"

# secondary prompt, printed when the shell needs more information to complete a command.
PS2='\`%_> '
# selection prompt used within a select loop.
PS3='?# '
# the execution trace prompt (setopt xtrace). default: '+%N:%i>'
PS4='+%N:%i:%_> '

zstyle ':completion:*' rehash true
source_if_exists "/usr/share/doc/pkgfile/command-not-found.zsh"
source_if_exists "/etc/zsh_command_not_found"
fpath=(/usr/local/share/zsh-completions $fpath)

# zsh syntax highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=('main' 'brackets' 'pattern')
source_if_exists "/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
