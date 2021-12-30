# Sensible command defaults for both bash and zsh.

# color on
set_alias diff "diff -uN --color=auto" diff
set_alias grep "grep --color=auto" grep
set_alias ip "ip --color=auto" ip

# drop-in replacements
set_alias cat "bat" bat
alias ls="ls -Fh --color=auto" && set_alias ls "exa -F" exa

# misspellings
set_alias gi git git
set_alias gt git git
alias sl=ls

# verbosity++
set_alias pkgfile "pkgfile -v" pkgfile

# get interface public ipv4 address
# https://major.io/icanhazip-com-faq/
set_alias icanhazip "curl -4 https://icanhazip.com/" curl
