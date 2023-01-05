# Custom user environment variables for both bash and zsh.

# Sensible $PATH should include user binary directories.
path_munge "$HOME/.bin" "$HOME/bin"

# scoop package manager
[[ "$(uname -s)" == "MSYS"* ]] && path_munge "$HOME/scoop/shims"

# Colorize CLI output when supported.
export CLICOLOR=1

# Set preferred text editor.
hash vim >/dev/null 2>&1 && export EDITOR="vim" VISUAL="vim"
