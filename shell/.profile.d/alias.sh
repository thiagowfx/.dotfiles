# Sensible command defaults for both bash and zsh.

# color on
alias diff="diff -uN --color=auto"
alias grep="grep --color=auto"
alias ip="ip --color=auto"

# drop-in replacements
alias ls="ls -Fh --color=auto" && hash exa >/dev/null 2>&1 && alias ls="exa -F --group-directories-first"

# misspellings
alias gi=git
alias gt=git
alias sl=ls

# https://frantic.im/cdtmp/
# Usage: cdtmp [foo]
cdtmp() {
	cd $(mktemp -d "/tmp/$USER-${1:+$1-}XXXXXX")
}
