#!/bin/bash
# shellcheck source=/dev/null

# Silence macOS bash deprecation warning.
export BASH_SILENCE_DEPRECATION_WARNING=1

# Abort on non-interactive shells.
# This must be the first entry because it is sourced from ~/.bash_profile.
[[ $- != *i* ]] && return

# History: Ignore entries that start with space and ignore duplicates.
HISTCONTROL="ignoreboth"

# History: Set unlimited size. Otherwise manually set to 50000.
HISTSIZE=
HISTFILESIZE=

# History: Use standard ISO 8601 timestamp.
#   %F is equivalent to %Y-%m-%d
#   %T is equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='[%F %T] '

# History: Default commands not to record.
HISTIGNORE="clear:history:ls:pwd:tree"

# Complete filenames after flag arguments.
#   https://stackoverflow.com/a/33740951/1745064
#
#   $ foo --config=$HOME/.b| <TAB> would expand .bashrc.
complete -D -o default 2&>/dev/null

# Enable history expansion with space.
#   Typing !!<SPC> will replace !! with the last command.
bind Space:magic-space

# Prepend cd to directory names automatically.
shopt -s autocd 2&>/dev/null

# Correct spelling errors in arguments supplied to cd.
shopt -s cdspell

# Correct spelling errors during tab-completion.
shopt -s dirspell 2&>/dev/null

# Turn on recursive globbing: Enables ** to recurse all directories.
shopt -s globstar 2&>/dev/null

# Append to the history file, do not overwrite it.
shopt -s histappend

# If set, and Readline is being used, a user is given the opportunity to re-edit a failed history substitution.
shopt -s histreedit

# If set, and Readline is being used, the results of history substitution are not immediately passed to the shell parser.
# Instead, the resulting line is loaded into the Readline editing buffer, allowing further modification.
shopt -s histverify

# Source base shell functions.
[ -r ~/.profilerc ] && . ~/.profilerc

# bash completion
src_files "/etc/bash_completion"

# Load user scripts and functions if existing. Order is important.
# Corp config is handled as part of .bashrc.d.
src_files "$HOME/.profile.d" "$HOME/.bashrc.d"

# Set nvim as vim after PATH is configured.
# This must be here (not in .profilerc) because Homebrew's PATH is added via
# .profile.d/10_brew.sh, which runs after .profilerc is sourced.
command -v nvim >/dev/null 2>&1 && export EDITOR="nvim" VISUAL="nvim" && alias vim=nvim

# Use starship prompt if available, otherwise use custom prompt.
if command -v starship &> /dev/null; then
    eval "$(starship init bash)"
else
    # Monokai-inspired: https://gist.github.com/transat/6694554
    RESET=$'\e[0m'
    BOLD=$'\e[1m'
    GREEN=$'\e[32;49m'
    ORANGE=$'\e[33;49m'
    RED=$'\e[31;49m'

    prompt_command() {
            # this must be the first line
            local EXIT="$?"

            # append command to history
            history -a

            # start with blank prompt
            PS1="\\[$RESET\\]"

            # add exit code from previous command if unsuccessful
            [ $EXIT != 0 ] && PS1+="\\[$BOLD\\]\\[$RED\\]$EXIT "

            # add user non-root as blue, root as red
            if [ "$(id -u)" -eq 0 ]; then
                    PS1+='\[$BOLD\]\[$RED\]\u'
            else
                    PS1+='\[$BOLD\]\[$GREEN\]\u'
            fi

            # add hostname and directory
            PS1+='\[$RESET\]@\h \[$BOLD\]\w\[$RESET\]'

            # add vanilla git prompt if existing
            # __git_ps1 is provided by the git package
            hash __git_ps1 &>/dev/null && PS1+='\[$GREEN\]$(__git_ps1)'

            # add prompt
            PS1+='\n\[$ORANGE\]\$ \[$RESET\]'

            # support OSC7 for VTE-based terminals (e.g. tilix) if existing
            hash __vte_osc7 &>/dev/null && __vte_osc7
    }
    PROMPT_COMMAND="prompt_command"
fi
