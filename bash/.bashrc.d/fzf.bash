#!/bin/bash

# fzf: fuzzy file finder
# https://github.com/junegunn/fzf#setting-up-shell-integration
hash fzf >/dev/null 2>&1 && eval "$(fzf --bash)"
