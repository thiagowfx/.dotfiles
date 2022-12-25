# Sensible command defaults for both bash and zsh.

# color on
alias diff="diff -uN --color=auto"
alias grep="grep --color=auto"
alias ip="ip --color=auto"

# verbose on
alias rsync="rsync -v"

# drop-in replacements
alias ls="ls -Fh --color=auto" && hash exa >/dev/null 2>&1 && alias ls="exa -F --group-directories-first"
alias la="ls -la --color=auto"

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
