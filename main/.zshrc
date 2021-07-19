#!/bin/zsh

# Prepend cd to directory names automatically.
setopt autocd

# Automatically list choices on an ambiguous completion.
setopt auto_list

# Do not beep on error in ZLE.
setopt nobeep

# Try to correct the spelling of commands.
setopt correct

# Use emacs keybindings.
#   List all keybindings with `bindkey`.
setopt emacs

# When this option is set and the default zsh-style globbing is in effect,
# the pattern ‘**/*’ can be abbreviated to ‘**’ and the pattern ‘***/*’
# can be abbreviated to ***. Hence ‘**.c’ finds a file ending in .c in
# any subdirectory, and ‘***.c’ does the same while also following symbolic
# links. A / immediately after the ‘**’ or ‘***’ forces the pattern to be
# treated as the unabbreviated form.
setopt glob_star_short

# Rehash new executables. If this causes performance issues on completion,
# disable it and just run `rehash` manually.
zstyle ':completion:*' rehash true

# Show completion menu.
zstyle ':completion:*' menu select

# Enable autocompletion.
autoload -Uz compinit && compinit

# Use `C-x C-e` to edit current command in $EDITOR with multi-line support.
# Saving and quitting $EDITOR returns to command prompt with the edited command
# inserted, but does not execute it until ENTER is pressed.
# https://unix.stackexchange.com/q/6620
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# Initialize prompt.
#   prompt -l to list all themes.
#   prompt -p to preview all themes.
autoload -Uz promptinit && promptinit

autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
[[ -n "${key[Up]}"   ]] && bindkey -- "${key[Up]}"   up-line-or-beginning-search
[[ -n "${key[Down]}" ]] && bindkey -- "${key[Down]}" down-line-or-beginning-search

# Asssitant functions for help command.
# E.g. git commit <Esc h> will open the man page of `git commit` instead of `git`.
autoload -Uz run-help-git run-help-p4
# Source base shell functions.
[ -r ~/.shellrc ] && . ~/.shellrc

# Load user scripts and functions if existing. Order is important.
src_files "$HOME/.shell.d" "$HOME/.zshrc.d"

# Load corp configs if any.
src_files "$HOME/.zshrc_corp"
