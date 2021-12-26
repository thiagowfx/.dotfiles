# Custom user environment variables for both bash and zsh.

# Sensible $PATH should include user binary directories.
path_munge "$HOME/.bin" "$HOME/bin"

# Colorize CLI output when supported.
add_env CLICOLOR 1

# Set preferred text editor.
add_env EDITOR "vim" vim
add_env VISUAL "vim" vim

# Set preferred terminal emulator, most preferred ones come last.
add_env TERMINAL "tilix" tilix
add_env TERMINAL "alacritty" alacritty
