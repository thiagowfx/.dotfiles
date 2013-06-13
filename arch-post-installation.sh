# Packages to install. I'm trying to be minimalist here.
sudo pacman -S w3m emacs-nox git fluxbox xterm pcmanfm feh zathura 
ranger texlive-bin racket

# Alterar mirrorlist ==> colocar os servidores brasileiros lá em cima, senão ficará preso em 50kb/s.
sudo emacs -nw /etc/pacman.d/mirrorlist

# Alterar o mapa de teclas para o teclado brasileiro
sudo setxkbmap br

# Alterar o locale para o brasileiro (descomentar a linha apropriada)
sudo emacs -nw /etc/locale.gen

