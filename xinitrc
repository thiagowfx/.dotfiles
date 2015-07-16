#!/bin/sh
#
# Example usage:
#   WM=i3 startx
# Use either xinit or startx.
#

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

#
# start a program automatically if 
# 1) it is installed on the system
# 2) an (optional) file provided as argument exists on the system
# 3) it is not runing already
#
# Arguments:
#   $1: what should be executed
#   $2: program that should exist (e.g. vim)
#   $3: an optional file (full-path) (e.g. $HOME/.vimrc)
#
startprog() {
    [[ "x$2" != "x" ]] && ! command -v "$2" &>/dev/null && return
    command -v pidof &>/dev/null && pidof "$2" &>/dev/null && return
    [[ "x$3" != "x" ]] && [ ! -e "$3" ] && return
    eval "$1" &
}

_tiling_f() {
    # X keymap: can be permanently set with `localectl set-x11-keymap` (part of systemd)
    startprog "setxkbmap br; setxkbmap -option terminate:ctrl_alt_bksp" setxkbmap

    # syndaemon: no touchpad clicking while typing
    startprog "syndaemon -i 1 -t -k -d" syndaemon

    # unclutter: hide the mouse when it gets idle for a while
    startprog "unclutter" unclutter

    # Xresources
    startprog "xrdb -merge $HOME/.Xresources" xrdb "$HOME/.Xresources"

    # X11 clipboard syncing
    # startprog "autocutsel -fork; autocutsel -selection PRIMARY -fork" autocutsel
    startprog "parcellite" parcellite

    # pulseaudio
    startprog "start-pulseaudio-x11" start-pulseaudio-x11

    # compositing manager
    startprog "xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55" xcompmgr

    # redshift (color temperature adjustment)
    startprog "redshift -l -23:-43" redshift

    # network manager applet
    startprog "nm-applet" nm-applet

    # Xmodmap
    startprog "xmodmap $HOME/.xmodmap" xmodmap "$HOME/.xmodmap"

    # feh: set desktop background
    startprog "sh $HOME/.fehbg" feh "$HOME/.fehbg"
}

bspwm_f() {
    # bind keys
    startprog "sxhkd" sxhkd

    _tiling_f
}

i3wm_f() {
    _tiling_f
}

# WM: The window manager to be started.
WM=${WM:-i3}

case $WM in
    awesome           ) exec awesome;;
    bspwm             ) bspwm_f; exec bspwm;;
    cinnamon          ) exec cinnamon-session;;
    dwm               ) exec dwm;;
    e17|enlightenment ) exec enlightenment_start;;
    fluxbox           ) exec startfluxbox;;
    gnome             ) exec gnome-session;;
    i3|i3wm           ) i3wm_f; exec i3;;
    kde               ) exec startkde;;
    lxde              ) exec startlxde;;
    mate              ) exec mate-session;;
    openbox           ) exec openbox-session;;
    unity             ) exec unity;;
    xfce|xfce4        ) exec startxfce4;;
    xmonad            ) exec xmonad;;
    *                 ) exec "$WM";;
esac
