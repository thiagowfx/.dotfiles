# config file
[[ -f ~/.bashrc ]] && . ~/.bashrc

# autostart X
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
