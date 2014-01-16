ppas=(
"ppa:videolan/stable-daily"
"ppa:me-davidsansome/clementine"
"ppa:mozillateam/thunderbird-stable"
"ppa:ubuntu-wine/ppa"
"ppa:webudp8team/y-ppa-manager"
"ppa:danielrichter2007/grub-customizer"
"ppa:noobslab/mint"
"ppa:lyx-devel/daily"
"ppa:libreoffice/ppa"
"ppa:elementary-os/daily"
"ppa:kubuntu-ppa/backports"
)

for ppa in $ppas; do
    sudo add-apt-repository $ppa
done
