#!/bin/sh

# nvm: node version manager
# https://github.com/nvm-sh/nvm
# bash_completion is intentional
export NVM_DIR="$HOME/.nvm"
# shellcheck disable=SC1091
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" && src_files "$NVM_DIR/bash_completion"
