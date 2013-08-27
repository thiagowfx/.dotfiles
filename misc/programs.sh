# VLSub: VLC extension to download subtitles
mkdir -p ~/.local/share/vlc/lua/extensions && cd $_
wget https://raw.github.com/exebetche/vlsub/master/vlsub.lua

# set default file manager (ex. for Dropbox)
xdg-mime default Thunar.desktop inode/directory
