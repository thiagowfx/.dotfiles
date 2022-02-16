#!/bin/bash

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
hash direnv >/dev/null 2>&1 && eval "$(direnv hook bash)"

# zoxide is a smarter cd command
# https://github.com/ajeetdsouza/zoxide
hash zoxide >/dev/null 2>&1 && eval "$(zoxide init bash)"
