# Custom user environment variables for both bash and zsh.

# Sensible $PATH should include user binary directories.
path_munge "$HOME/.bin" "$HOME/bin"

# golang
path_munge "$HOME/go/bin"

# rust / cargo
path_munge "$HOME/.cargo/bin"

# scoop package manager
[[ "$(uname -s)" == "MSYS"* ]] && path_munge "$HOME/scoop/shims"

# Colorize CLI output when supported.
export CLICOLOR=1

# Force the system-wide language to English.
# Both "en_US" and "en_CA" work here.
# We could also optionally set LC_ALL but it is not necessary.
# Verify current locale settings with `locale`.
export LANG="en_US"

# https://stackoverflow.com/questions/46288847/how-to-suppress-pip-upgrade-warning
hash python3 >/dev/null 2>&1 && export PIP_DISABLE_PIP_VERSION_CHECK=1

# Set preferred text editor.
hash vim >/dev/null 2>&1 && export EDITOR="vim" VISUAL="vim"
