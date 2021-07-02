#!/bin/bash
# Custom user environment variables

# Colorize CLI output when supported.
add_env CLICOLOR 1

# Set text editors.
add_env EDITOR "vim" vim
add_env VISUAL "vim" vim

# Add color support to less: --raw-control-chars
add_env LESS "-R" less

# Colored man pages.
# https://wiki.archlinux.org/index.php/Color_output_in_console#man
add_env LESS_TERMCAP_md $'\e[01;31m'
add_env LESS_TERMCAP_me $'\e[0m'
add_env LESS_TERMCAP_se $'\e[0m'
add_env LESS_TERMCAP_so $'\e[01;44;33m'
add_env LESS_TERMCAP_ue $'\e[0m'
add_env LESS_TERMCAP_us $'\e[01;32m'

# Display progress percentage in man pages.
# https://unix.stackexchange.com/a/329092/41858
add_env MANPAGER "less -s -M +Gg" less
