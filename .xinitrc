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
setxkbmap br

# xinit window_manager
case $1 in
    gnome) exec gnome-session;;
    kde) exec startkde;;
    cinnamon) exec cinnamon;;
    awesome) exec awesome;;
    openbox) exec openbox-session;;
    fluxbox) exec startfluxbox;;
    xfce|*) exec startxfce4;;
esac
