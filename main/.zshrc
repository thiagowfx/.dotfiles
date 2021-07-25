#!/bin/zsh
# zsh uses its own ZLE (Zsh Line Editor) instead of readline (~/.inputrc).
#
# Prompt theme:
#   prompt -l to list all themes.
#   prompt -p to preview all themes.
#
# Useful tips:
#   ESC h to call man for the current command line.

# Plugin manager for zsh: https://getantibody.github.io/
# Plugin order is important.
if hash "antibody" >/dev/null 2>&1; then
	source <(antibody init)

	antibody bundle grml/grml-etc-core
	source "$(antibody path grml/grml-etc-core)/etc/zsh/zshrc"

	# Fish shell-like autosuggestions for zsh
	# Suggest commands as you type based on history and completions.
	#   Default: history only.
	#   Use C-e or C-f to accept a suggestion.
	antibody bundle zsh-users/zsh-autosuggestions
	ZSH_AUTOSUGGEST_STRATEGY=(history completion)

	# Fish shell-like syntax highlighting for Zsh.
	antibody bundle zsh-users/zsh-syntax-highlighting

	# Fish shell-like history search (up arrow).
	# Load it after zsh-users/zsh-syntax-highlighting.
	antibody bundle zsh-users/zsh-history-substring-search
	bindkey '^[[A' history-substring-search-up   # up arrow
	bindkey '^[[B' history-substring-search-down # down arrow
	bindkey '^P' history-substring-search-up
	bindkey '^N' history-substring-search-down
fi

# Automatically list choices on an ambiguous completion.
setopt auto_list

# When this option is set and the default zsh-style globbing is in effect,
# the pattern ‘**/*’ can be abbreviated to ‘**’ and the pattern ‘***/*’
# can be abbreviated to ***. Hence ‘**.c’ finds a file ending in .c in
# any subdirectory, and ‘***.c’ does the same while also following symbolic
# links. A / immediately after the ‘**’ or ‘***’ forces the pattern to be
# treated as the unabbreviated form.
setopt glob_star_short

# Use `C-x C-e` to edit current command in $EDITOR with multi-line support.
# Saving and quitting $EDITOR returns to command prompt with the edited command
# inserted, but does not execute it until ENTER is pressed.
# https://unix.stackexchange.com/q/6620
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# Compatibility with bash completion.
autoload -Uz bashcompinit && bashcompinit

# Source base shell functions.
[ -r ~/.shellrc ] && . ~/.shellrc

# Load user scripts and functions if existing. Order is important.
src_files "$HOME/.profile.d" "$HOME/.zshrc.d"

# Load corp configs if any.
src_files "$HOME/.zshrc_corp"
