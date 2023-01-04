# Sensible command defaults for both bash and zsh.

# color on
alias diff="diff -uN --color=auto"
alias grep="grep --color=auto"
alias ip="ip --color=auto"

# verbose on
alias rsync="rsync -v"
alias ls="ls -Fh --color=auto"
alias la="ls -la --color=auto"
alias l="ls -l --color=auto" && alias ll="l"

# exa: modern drop-in replacement for ls
if hash exa >/dev/null 2>&1; then
	alias ls="exa -F --group-directories-first"
fi

# misspellings
alias gi=git
alias gt=git
alias sl=ls

# muscle memory
alias unstow="stow -D"

# Inspiration from https://frantic.im/cdtmp/ and grml-zsh-config's cdt
# Usage: cdtmp [foo]
cdtmp() {
	builtin cd $(mktemp -d "/tmp/$USER-${1:+$1-}XXXXXX")
	builtin pwd
}
