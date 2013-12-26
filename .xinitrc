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

xsetroot -cursor_name left_ptr
setxkbmap br

# xinit window_manager
case $1 in
    awesome) exec awesome;;
    catwm) exec catwm;;
    cinnamon) exec cinnamon-session;;
    ede) exec startede;;
    fluxbox) exec startfluxbox;;
    gnome) exec gnome-session;;
    gnome-classic) exec gnome-session --session=gnome-classic;;
    i3) exec i3;;
    icewm) exec icewm-session;;
    e17|enlightenment) exec enlightenment_start;;
    jwm) exec jwm;;
    kde) exec startkde;;
    mate) exec mate-session;;
    monster|monsterwm) exec monsterwm;;
    notion) exec notion;;
    openbox) exec openbox-session;;
    xfce|xfce4) exec startxfce4;;
    xmonad) exec xmonad;;
    unity) exec unity;;
    *) exec cinnamon-session;;
esac
