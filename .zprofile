# config file
[[ -f ~/.zshrc ]] && . ~/.zshrc

# autostart X
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
