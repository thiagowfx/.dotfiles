# Sensible command defaults for both bash and zsh.

# color on
set_alias diff "diff -uN --color=auto" diff
set_alias grep "grep --color=auto" grep
set_alias ip "ip --color=auto" ip

# drop-in replacements
alias ls="ls -Fh --color=auto" && set_alias ls "exa -F" exa

# misspellings
set_alias gi git git
set_alias gt git git
alias sl=ls

# verbosity++
set_alias pkgfile "pkgfile -v" pkgfile
