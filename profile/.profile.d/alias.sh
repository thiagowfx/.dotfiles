#!/bin/sh
# Sensible command defaults for both bash and zsh.

# color on
alias diff="diff -uN --color=auto"
alias grep="grep --color=auto"

# verbose on
alias ls="ls -Fh --color=auto"
alias la="ls -la --color=auto"
alias l="ls -l --color=auto"
alias ll="l"

# eza: modern drop-in replacement for ls
if command -v eza >/dev/null 2>&1; then
        alias ls="eza -F --group-directories-first"
fi

# https://direnv.net/
if command -v direnv >/dev/null 2>&1; then
	alias de="direnv edit .envrc.local 2>/dev/null || direnv edit .envrc"
fi

# misspellings
alias t=git
alias it=git
alias gi=git
alias gt=git
alias gti=git
alias sl=ls

alias cdg='cd "$(git root)"'

# shortcuts
# exit everything: quit all nested shells, closing the terminal tab
ee() {
	pid=$$
	pids=$pid
	while ppid=$(ps -o ppid= -p "$pid" | tr -d ' ') && [ "$ppid" -gt 1 ] 2>/dev/null; do
		case "$(ps -o comm= -p "$ppid" 2>/dev/null)" in
			*sh) pids="$pids $ppid"; pid=$ppid ;;
			*) break ;;
		esac
	done
	eval "kill -HUP $pids"
}

if command -v kubectl >/dev/null 2>&1; then
	alias k=kubectl
fi

if command -v cco >/dev/null 2>&1; then
	alias claudey="cco --allow-oauth-refresh --add-dir ~/.cache --add-dir ~/.aws/cli/cache --add-dir ~/.terraform.d/plugin-cache"
else
	alias claudey="claude --dangerously-skip-permissions"
fi

# muscle memory
alias unstow="stow -D"

# editor: prefer nvim over vim
# (must be in alias.sh, not .profilerc, because Homebrew PATH isn't set yet in .profilerc)
if command -v nvim >/dev/null 2>&1; then
        export EDITOR="nvim" VISUAL="nvim"
        alias vim=nvim
elif command -v vim >/dev/null 2>&1; then
        export EDITOR="vim" VISUAL="vim"
fi
