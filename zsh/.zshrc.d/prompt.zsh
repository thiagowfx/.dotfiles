#!/bin/zsh
# Set zsh prompt.

# Set python-venv token.
python_venv_prompt() {
    REPLY=${VIRTUAL_ENV+(${VIRTUAL_ENV:t}) }
}
grml_theme_add_token python-venv -f python_venv_prompt '%F{magenta}' '%f'

# Customize grml prompt.
#   Two lines instead of one
#   Add python-venv token
zstyle ':prompt:grml:left:items:user' pre '%B%F{green}'
zstyle ':prompt:grml:left:setup' items rc python-venv change-root user at host path vcs newline percent

