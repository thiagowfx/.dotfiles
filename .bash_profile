# just maintain one file
[[ -f ~/.bashrc ]] && . ~/.bashrc

# autologin on X
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
