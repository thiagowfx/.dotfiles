#!/bin/bash
# Find the TTY by walking up the process tree
find_tty() {
    local pid=$$
    local tty
    while [ "$pid" != "1" ]; do
        tty=$(ps -p "$pid" -o tty= 2>/dev/null | tr -d ' ')
        if [ -n "$tty" ] && [ "$tty" != "??" ]; then
            echo "/dev/$tty"
            return
        fi
        pid=$(ps -p "$pid" -o ppid= 2>/dev/null | tr -d ' ')
    done
}

TTY=$(find_tty)
if [ -n "$TTY" ] && [ -e "$TTY" ]; then
    printf '\a' > "$TTY"
fi

/usr/bin/afplay /System/Library/Sounds/Glass.aiff &
