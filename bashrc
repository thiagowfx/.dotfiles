# -*- shell-script -*-

# if not running interactively, don't do anything
[[ $- != *i* ]] && return

source_if_exists() {
    [[ -f "$@" ]] && source "$@"
}

# aliases
source_if_exists "$HOME/.common.sh"

export HISTCONTROL="ignoreboth" # ignorespace and ignoredups
export HISTSIZE=50000000
export HISTFILESIZE="$HISTSIZE"
export HISTIGNORE="ls:cd:cd -:cd ~:pwd:exit:date:* --help"

# autojump for debian, arch and OS X
source_if_exists "/usr/share/autojump/autojump.bash"
source_if_exists "/etc/profile.d/autojump.bash"
source_if_exists "/opt/local/etc/profile.d/autojump.sh"
command -v brew &>/dev/null && source_if_exists "$(brew --prefix autojump)/etc/autojump.sh"

# system bash configs
source_if_exists "/etc/bashrc"
source_if_exists "/etc/bash_completion"
command -v brew &>/dev/null && source_if_exists "$(brew --prefix)/share/bash-completion/bash_completion"

# completion for sudo
complete -cf sudo

# command-not-found hook: arch
source_if_exists "/usr/share/doc/pkgfile/command-not-found.bash"

# prompt
git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1) /'; }
PS1="\[\e[0;32m\]\$(git_branch)\[\e[m\]\[\033[01;32m\]\u@\h\[\033[00m\] \[\033[01;34m\]\W\[\033[00m\]\[\033[1;32m\] \$\[\033[m\] "
source_if_exists "$HOME/.bash_prompt"

# shell options
set -o emacs
shopt -s checkwinsize
shopt -s cdspell
shopt -s cmdhist
shopt -s dotglob
shopt -s expand_aliases
shopt -s extglob
shopt -s histappend
shopt -s hostcomplete
shopt -s nocaseglob

# colored man pages
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;38;5;74m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[38;5;246m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;38;5;146m' \
        man "$@"
}
