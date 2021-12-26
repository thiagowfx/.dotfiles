# Sensible command defaults for both bash and zsh.

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

# verbosity++
add_alias pkgfile "pkgfile -v" pkgfile

# get interface public ipv4 address
# https://major.io/icanhazip-com-faq/
add_alias icanhazip "curl -4 https://icanhazip.com/" curl
