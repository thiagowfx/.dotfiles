# just maintain one file
[[ -f ~/.zshrc ]] && . ~/.zshrc

# autologin on X
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
