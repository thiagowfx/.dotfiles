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
	alias de='if [ -f .envrc.local ]; then direnv edit .envrc.local; else direnv edit .envrc; fi'
fi

# misspellings
alias t=git
alias it=git
alias gi=git
alias gt=git
alias gti=git
alias sl=ls

cdg() {
	# This file is sourced by bash and zsh, both of which support local.
	# shellcheck disable=SC3043
	local line main_is_bare main_worktree root worktrees

	root=$(git rev-parse --path-format=absolute --show-toplevel) || return

	if [ "$(pwd -P)" != "$root" ]; then
		cd "$root" || return
		return
	fi

	worktrees=$(git worktree list --porcelain) || return
	main_worktree=
	main_is_bare=false
	while IFS= read -r line; do
		case "$line" in
			worktree\ *) main_worktree=${line#worktree } ;;
			bare) main_is_bare=true ;;
			'') break ;;
		esac
	done <<-EOF
	$worktrees
	EOF

	if [ "$main_is_bare" = false ] && [ -n "$main_worktree" ] && [ "$main_worktree" != "$root" ]; then
		cd "$main_worktree" || return
	fi
}

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

if command -v obsidian >/dev/null 2>&1; then
	alias ob=obsidian
fi

if command -v claude >/dev/null 2>&1; then
	if command -v cco >/dev/null 2>&1; then
		alias claudey="cco --allow-oauth-refresh --add-dir ~/.cache --add-dir ~/.aws/cli/cache --add-dir ~/.aws/sso/cache --add-dir ~/.azure --add-dir ~/.terraform.d/plugin-cache --add-dir ~/go --add-dir ~/Library/Keychains"
	fi
	alias claudeyy="claude --allow-dangerously-skip-permissions"
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
