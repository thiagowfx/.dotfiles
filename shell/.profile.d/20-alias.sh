# Sensible command defaults

# color on
add_alias diff "diff -uN --color=auto" diff
add_alias grep "grep --color=auto" grep
add_alias ip "ip --color=auto" ip

# drop-in replacements
add_alias ls "ls -Fh --color=auto" ls && add_alias ls "exa -F" exa

# misspellings
add_alias gi git git
add_alias gt git git
add_alias sl ls

# remember more easily
add_alias unstow "stow -D" stow

# sensible defaults
add_alias dtrx "dtrx -n" dtrx
add_alias fpp "fpp -ko" fpp

# verbosity++
add_alias pkgfile "pkgfile -v" pkgfile
