echo "First, let's get aptitude, which is a little better than apt-get"
sudo apt-get -y update && sudo apt-get -y install aptitude
echo "aptitude package manager installed".

echo "Then, let's do a little refreshing"
sudo aptitude -y update && sudo aptitude -y upgrade
echo "Repos updated and upgraded."

echo "Now let's remove some unnecessary (a.k.a.) bloat programs)"
sudo aptitude remove -y unity-lens-shopping rhythmbox evolution totem transmission-gtk transmission-common
echo "Unnecessary programs removed."

echo "Install several programs and packages"
sudo aptitude install zram-config calibre anki emacs emacs-goodies-el gparted p7zip guake p7zip-rar virtualbox tomboy nfoview unison-gtk unison krename chmsee bluefish kdiff3 geogebra carmetal kolourpaint4 xchat ttf-mscorefonts-installer winff pingus inkscape blender kile gwenview gimp lyx gnucash gnucash retext pdfsam calligraflow deluge wxmaxima clementine audacity vlc smplayer easytag gpodder avidemux kdenlive winff vim mtpfs mtp-tools dropbox g++ openjdk-7-jre eclipse git git-core apt-file dropbox compizconfig-settings-manager unity-tweak-tool skype scribus gnome-tweak-tool mozplugger gedit-plugins clipit w3m youtube-dl ubuntu-restricted-extras racket wine winetricks playonlinux bleachbit flashplugin-installer gsfonts-x11 indicator-multiload libavformat-extra-53 libavcodec-extra-53 w32-codecs non-free-codecs dia radiotray scilab nautilus-open-terminal unity-2d indent markdown python-progressbar idle openshot kipi-plugins unity-webapps-googlecalendar meld pcmanfm conky gcalctool gimp-data-extras baobab minitube nemo
echo "Packages installed".

echo "Some optional programs"
sudo apt-get install unity-lens-* unity-scope-* exo-utils xpaint idraw xfig tgif bitmap tuxpaint dolphin
echo "Optional programs installed"

echo "Some shell utils"
sudo aptitude install htop feh nmap ranger zathura traceroute 
echo "Utils installed."

================================================================================

echo "Install some PPAs"
# VLC Media Player
sudo add-apt-repository ppa:videolan/stable-daily

# Oracle Java
sudo add-apt-repository ppa:webupd8team/java

# Wine
sudo add-apt-repository ppa:ubuntu-wine/ppa

# Mozilla Thunderbird
sudo add-apt-repository ppa:mozillateam/thunderbird-stable

# Y PPA Manager
sudo add-apt-repository ppa:webupd8team/y-ppa-manager

# Indicator Multiload
sudo add-apt-repository ppa:indicator-multiload/stable-daily

# Kubuntu Backports (caution here!)
sudo add-apt-repository ppa:kubuntu-ppa/backports

# Grub Customizer
sudo add-apt-repository ppa:danielrichter2007/grub-customizer

# Docky / Plank
sudo apt-add-repository ppa:ricotz/docky

# RazorQt
sudo add-apt-repository ppa:razor-qt

# Unity Experimental
# http://ubuntued.info/unity-smart-scopes-tenha-seu-unity-bastante-mais-inteligente
# sudo add-apt-repository ppa:ubuntu-unity/experimental-prevalidation

# NoobsLab
# http://www.noobslab.com/2013/06/nemo-file-manager-for-ubuntu.html
sudo add-apt-repository ppa:noobslab/mint

# LyX Document Processor
sudo add-apt-repository ppa:lyx-devel/daily

# Compiz
# I don't recommend you adding this PPA. I had some issues (trouble!) with it.
# sudo add-apt-repository ppa:compiz/ppa

# PlayStation Emulator o/
# sudo add-apt-repository ppa:noobslab/pcsx2

# Pidgin Messenger
# sudo add-apt-repository ppa:pidgin-developers/ppa

# Web UDP 8 Team
# sudo add-apt-repository ppa:nilarimogard/webupd8

# Gnome 3 PPA
# sudo add-apt-repository ppa:gnome3-team/gnome3

# Deluge BitTorrent Client
# sudo add-apt-repository ppa:deluge-team/ppa
echo "PPAs added"

echo "Installing some PPAs programs"
sudo aptitude install oracle-jdk7-installer y-ppa-manager
echo "Programs installed"

# Install proprietary drivers
# You should search the right video driver for your card. See `lspci | grep VGA`
# sudo aptitude install nvidia-current nvidia-settings

echo "Clean the apt cache"
sudo apt-get -y autoremove >>/dev/null && sudo apt-get -y autoclean >>/dev/null
echo "Cache cleaned."
