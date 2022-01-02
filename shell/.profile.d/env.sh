# Custom user environment variables for both bash and zsh.

# Sensible $PATH should include user binary directories.
path_munge "$HOME/.bin" "$HOME/bin"

# Colorize CLI output when supported.
export CLICOLOR=1

# Set preferred text editor.
set_env EDITOR vim
set_env VISUAL vim

# https://github.com/funtoo/keychain
hash keychain >/dev/null 2>&1 && eval $(keychain --quiet --eval id_mercury id_water 2&>/dev/null)
