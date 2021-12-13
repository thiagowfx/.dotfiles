#!/bin/sh
#
# Sourced by startx(1), xinit(1).
#
# https://wiki.archlinux.org/title/Xinit

# Load Xmodmap.
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

# Load Xresources.
[ -f ~/.Xresources ] && xrdb -merge ~/.Xresources

# System xinitrc.d.
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    # shellcheck source=/dev/null
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Load xprofile.
# shellcheck source=/dev/null
[ -r ~/.xprofile ] && . ~/.xprofile

# Start window manager / session.
#   startx (default WM)
#   startx ~/.xinitrc <wm> (custom WM)
case ${1:-i3} in
  cinnamon)
    exec cinnamon-session;;
  i3*)
    exec i3;;
  xfce*)
    exec startxfce4;;
esac
