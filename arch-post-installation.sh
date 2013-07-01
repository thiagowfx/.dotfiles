## review please:
# xbindkeys
# maybe
# aurvote customizepkg

# update &  upgrade
sudo pacman -Syy && sudo pacman -Syu

# packages to install. I'm trying to be minimalist here.
sudo pacman -S emacs-nox git guake feh zathura ranger texlive-bin racket zsh tomboy gparted wget libreoffice-writer make base-devel gnome-calculator youtube-dl nemo

# Alterar mirrorlist ==> colocar os servidores brasileiros lá em cima, senão ficará preso em 50kb/s.
sudo emacs -nw /etc/pacman.d/mirrorlist
# mv /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.orig && rankmirrors -n 3 /etc/pacman.d/mirrorlist.orig > /etc/pacman.d/mirrorlist

# Alterar o mapa de teclas para o teclado brasileiro (adicionar isso ao startup do sistema)
setxkbmap br

# Alterar o locale para o brasileiro (descomentar a linha apropriada)
sudo emacs -nw /etc/locale.gen

# Install audio!!!
sudo pacman -S pulseaudio gstreamer0.10-{bad,base,ugly,good}-plugins gstreamer pulseaudio-alsa clementine

# utilities (pulseaudio)
# yaourt paman pavumeter pasystray

# start pulseaudio when X session starts
# echo "start-pulseaudio-x11" >> ~/.xinitrc

# pulseaudio --start       # start pulseaudio
# pulseaudio -k            # kill pulseaudio

# proprietary driver
# http://www2.ati.com/relnotes/catalyst_linux_installer.pdf
