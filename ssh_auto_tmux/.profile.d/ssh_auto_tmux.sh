# shellcheck disable=SC2148

# Automatically spawn tmux within SSH sessions for interactive terminals.
# https://stackoverflow.com/a/43819740/1745064
#
# The session is called `main`.
# Create a session with PREFIX :new, rename with PREFIX $, toggle with PREFIX s.
#
# Escape hatch:
#   ssh <host> -t -- NOTMUX=1 <shell>
if [ -z "$NOTMUX" ] && [ -z "$TMUX" ] && [ -n "$SSH_TTY" ] && [[ $- =~ i ]]; then
    tmux -u new -A -s main
    exit
fi
