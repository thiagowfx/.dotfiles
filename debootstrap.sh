files=(
    .Xmodmap
    .Xresources
    .aliases
    .autostart
    .bash_logout
    .bash_profile
    .bashrc
    .config/dunst/dunstrc
    .config/fontconfig/fonts.conf
    .config/mpd/mpd.conf
    .drirc
    .emacs
    .gdbinit
    .gitconfig
    .hgrc
    .i3/
    .i3blocks.conf
    .irssi/config
    .makepkg.conf
    .xinitrc
    .zprofile
    .zshrc
)

for f in "${files[@]}"; do
    ln -s "$HOME/.dotfiles/$f" "$HOME/$f"
done

sudo pacman -S - <meta/packages-pacman
