# Automatically spawn tmux within ssh sessions for interactive terminals.
# https://stackoverflow.com/a/43819740/1745064
#
# The session is called `main`.
# To create a new session, do PREFIX :new
#
# Use NOTMUX=1 as a escape hatch.
if [ -z "$NOTMUX" ] && [ -z "$TMUX" ] && [ -n "$SSH_TTY" ] && [[ $- =~ i ]]; then
    tmux new -A -s main
    exit
fi
