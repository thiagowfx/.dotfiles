#!/bin/bash

# homebrew bash completion
hash brew >/dev/null 2>&1 && src_files "$(brew --prefix)/etc/profile.d/bash_completion.sh"

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
hash direnv >/dev/null 2>&1 && eval "$(direnv hook bash)"

# gh is github's official command line tool
hash gh >/dev/null 2>&1 && eval "$(gh completion -s bash)"

# zoxide is a smarter cd command
# https://github.com/ajeetdsouza/zoxide
hash zoxide >/dev/null 2>&1 && eval "$(zoxide init bash)"
