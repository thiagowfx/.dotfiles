# Custom user environment variables

# Sensible $PATH
pathmunge "$HOME/.bin"
pathmunge "$HOME/bin"

# Colorize CLI output when supported.
add_env CLICOLOR 1

# Set text editors.
add_env EDITOR "vim" vim
add_env VISUAL "vim" vim

# sensible defaults for less: --RAW-CONTROL-CHARS --ignore-case
add_env LESS "-R -i" less

# Colored man pages: https://wiki.archlinux.org/title/Color_output_in_console#man
# Display progress percentage in man pages: https://unix.stackexchange.com/a/329092/41858
add_env MANPAGER "less -R --use-color -Dd+r -Du+b -s -M +Gg" less
