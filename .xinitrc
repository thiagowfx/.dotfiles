#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# set the X cursor
xsetroot -cursor_name left_ptr
# set Brazilian keyboard layout
setxkbmap -layout br

# xinit window_manager
case $1 in
    gnome) exec gnome-session;;
    kde) exec startkde;;
    xfce) exec startxfce4;;
    awesome) exec awesome;;
    openbox) exec openbox;;
    fluxbox|*) exec startfluxbox;;
esac
