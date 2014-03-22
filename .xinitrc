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

##################################################
# Keyboard
# setxkbmap br &                  # see also: localectl set-x11-keymap for systemd
# setxkbmap -option 'ctrl:swapcaps' # for emacs
# setxkbmap -option 'ctrl:nocaps'   # for emacs
xmodmap .Xmodmap
##################################################

# xsetroot -cursor_name left_ptr
# xset dpms 300 600 900 &
# xbacklight -set 50
synclient CoastingFriction=400 # increase the touchpad scrolling friction (this avoids excessive accidental zooming on Chromium)

# xinit <your_window_manager_here>
case $1 in
    awesome) exec awesome;;
    catwm) exec catwm;;
    cinnamon) exec cinnamon-session;;
    dwm) exec dwm;;
    enlightenment) exec enlightenment_start;;
    ede) exec startede;;
    fluxbox) exec startfluxbox;;
    gnome) exec gnome-session;;
    gnome-classic) exec gnome-session --session=gnome-classic;;
    i3|i3wm) exec i3;;
    icewm) exec icewm-session;;
    jwm) exec jwm;;
    kde) exec startkde;;
    mate) exec mate-session;;
    monster|monsterwm) exec monsterwm;;
    notion) exec notion;;
    openbox) exec openbox-session;;
    unity) exec unity;;
    xfce|xfce4) exec startxfce4;;
    xmonad) exec xmonad;;
    *) exec i3;;
esac
