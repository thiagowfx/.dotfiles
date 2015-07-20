#!/bin/sh

export BROWSER="firefox"
export TERMINAL="termite"

# GnuPG: password only once per session
export GPG_TTY=$(tty) GPG_AGENT_INFO=""

# autostart X on tty1
[[ -z "${DISPLAY}" && "${XDG_VTNR}" -eq 1 ]] && exec startx
