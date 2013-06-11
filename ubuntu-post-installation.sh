# First, let's get aptitude, which is a little better than apt-get
sudo apt-get -y install aptitude

# Then, let's do a little refreshing
sudo aptitude -y update && sudo aptitude -y upgrade
echo "Repos updated and upgraded."

# Remove unnecessary programs
sudo aptitude remove -y unity-lens-shopping rhythmbox evolution totem transmission-gtk transmission-common

## Install several programs, asking for the user confirmation (security issues)
sudo aptitude install calibre anki emacs emacs-goodies-el gparted p7zip guake p7zip-rar virtualbox tomboy nfoview unison-gtk unison krename chmsee bluefish kdiff3 geogebra carmetal kolourpaint4 xchat ttf-mscorefonts-installer winff pingus inkscape blender kile gwenview gimp lyx gnucash gnucash retext pdfsam calligraflow deluge wxmaxima clementine audacity vlc smplayer easytag gpodder avidemux kdenlive winff vim mtpfs mtp-tools dropbox g++ openjdk-7-jre eclipse git git-core apt-file dropbox compizconfig-settings-manager unity-tweak-tool skype scribus gnome-tweak-tool mozplugger gedit-plugins clipit w3m youtube-dl ubuntu-restricted-extras racket wine winetricks playonlinux bleachbit flashplugin-installer gsfonts-x11 indicator-multiload libavformat-extra-53 libavcodec-extra-53 w32-codecs non-free-codecs dia radiotray scilab nautilus-open-terminal unity-2d indent markdown python-progressbar idle openshot

# images
sudo aptitude install feh                   # gpicview
# network utilities
sudo aptitude install traceroute nmap
# shell
sudo aptitude install ranger                # file manager
echo "Programs installed."

## Others
# sc # spreadsheet in terminal

# Install some programs provided by PPAs
# sudo aptitude install zramswap-enabler oracle-jdk7-installer y-ppa-manager

# Install proprietary drivers
# sudo aptitude install nvidia-current nvidia-settings

# Clean the cache, hiding unwanted messages
sudo apt-get -y autoremove >>/dev/null && sudo apt-get -y autoclean >>/dev/null
echo "Cache cleaned."
