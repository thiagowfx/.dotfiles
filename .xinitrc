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
    unity) exec unity;;
    kde) exec startkde;;
    cinnamon) exec cinnamon;;
    openbox) exec openbox-session;;
    awesome) exec awesome;;
    fluxbox) exec startfluxbox;;
    xmonad) exec xmonad;;
    ede) exec startede;;
    i3) exec i3;;
    icewm) exec icewm-session;;
    jwm) exec jwm;;
    e17|enlightenment) exec enlightenment_start;;
    notion) exec notion;;
    xfce|xfce4) exec startxfce4;;
    monster|monsterwm) exec monsterwm;;
# default window manager
    *) exec startfluxbox;;
esac
