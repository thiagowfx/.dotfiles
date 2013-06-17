# Update, upgrade
sudo pacman -Syy
sudo pacman -Syu

# Packages to install. I'm trying to be minimalist here.
sudo pacman -S w3m emacs-nox git fluxbox xterm feh zathura 
ranger texlive-bin racket ranger zsh tomboy gparted wget

# thunar thunar-volman

# Alterar mirrorlist ==> colocar os servidores brasileiros lá em cima, senão ficará preso em 50kb/s.
sudo emacs -nw /etc/pacman.d/mirrorlist

# Alterar o mapa de teclas para o teclado brasileiro (adicionar isso ao startup do sistema)
setxkbmap br

# Alterar o locale para o brasileiro (descomentar a linha apropriada)
sudo emacs -nw /etc/locale.gen

