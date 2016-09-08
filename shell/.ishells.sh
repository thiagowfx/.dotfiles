# -*- shell-script -*-
# 
# ASSUMES: it is sourced from a shell where source_if_exists has been defined.
#

if [[ $TERM == xterm-termite ]]; then
      source_if_exists "/etc/profile.d/vte.sh"
fi

# Templates are functions/snippets for general use.
# They are aliased preceded with "t-".
#
# The first argument should be a function defined internally, preceded with _.
#
# A program can be optionally specified as second argument; if it is not found
# in the PATH, then the template won't be defined.
#
# Similarly, you can specify a file as third argument. If the file is not found,
# then the template won't be defined.
#
# Arguments:
#  $1: template name
#  $2: a program (e.g. vim or /usr/bin/vim)
#  $3: a file (e.g. /boot/grub/grub.cfg)
#
addtemplate() {
  [[ "x$2" != "x" ]] && ! command -v "$2" &>/dev/null && return
  [[ "x$3" != "x" ]] && [ ! -e "$3" ] && return
  alias "t-$1"="_$1"
}

# Creates an alias iff the specified program and/or a given file
# exists on the system.
#
# Arguments:
#  $1: the alias
#  $2: its value
#  $3: a program (e.g. vim or /usr/bin/vim)
#  $4: a file (e.g. $HOME/directory)
#
addalias() {
  [[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
  [[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
  alias "$1"="$2"
}

# Sets an environment variable iff its correlated program
# is installed and/or if a given file exists on the system.
#
# Arguments:
#  $1: the environment variable (e.g. EDITOR)
#  $2: its value
#  $3: a program (e.g. vim or /usr/bin/vim)
#  $4: a file (e.g. $HOME/directory)
#
addenv() {
    [[ "x$3" != "x" ]] && ! command -v "$3" &>/dev/null && return
    [[ "x$4" != "x" ]] && [ ! -e "$4" ] && return
    export "$1"="$2"
}
addpath() {
    addenv PATH "$1:$PATH" "$2" "${3:-$1}"
}
addwine() {
    addenv "$1" "$2" wine
}

addalias cower "cower --color=always --sort=votes" cower
addalias make "make -j"
addalias tmux "tmux -2" tmux
addalias xclip "xclip -selection clipboard" xclip

addalias chmod "chmod -v" chmod
addalias chown "chown -v" chown
addalias cp "cp -v" cp
addalias curl "curl -v -L" curl
addalias df "df -h" df
addalias du "du -h" du
addalias free "free -h" free
addalias grep "grep --color=always" grep
addalias hexdump "hexdump -C" hexdump
addalias ln "ln -v" ln
addalias mv "mv -v" mv
addalias netstat "netstat -pln" netstat
addalias pgrep "pgrep -fl" pgrep
addalias pstree "pstree -p" pstree

addalias ls "ls --color=always -F" ls
addalias sl "ls" ls
addalias l "ls -l" ls
addalias ll "l" ls

addalias diff "diff -uN" diff
addalias diff "colordiff -uN" colordiff
addalias colordiff "colordiff -uN" colordiff

addenv EDITOR "vim" vim
addenv VISUAL "$EDITOR"
addenv LESS "-R" less
addenv GTEST_COLOR "YES"

addpath "$HOME/bin"
addpath "$HOME/.bin"
addpath "/usr/lib/ccache/bin" ccache
addpath "/opt/local/bin" port
addpath "/opt/local/sbin" port
addpath "/usr/local/sbin"
command -v ruby &>/dev/null && addpath "$(ruby -rubygems -e "puts Gem.user_dir")/bin"

addwine WINEARCH "win32"
addwine WINEDEBUG "fixme-all"
addwine WINEDLLOVERRIDES "winemenubuilder.exe=d"


# upgrade an Unix / Linux system
_world() {
    [[ "$(uname)" == "Darwin" ]] && command -v brew &>/dev/null && brew update && brew upgrade --all && brew cleanup && return
    [[ "$(uname)" == "Darwin" ]] && command -v port &>/dev/null && sudo port selfupdate && sudo port upgrade outdated && return

    [[ -e /etc/arch-release ]] && sudo pacman -Syu "$@" && return
    [[ -e /etc/debian_version ]] && sudo apt update && sudo apt full-upgrade && return
    [[ -e /etc/gentoo-release ]] &&  su -c "emerge --sync && emerge -avuDN --with-bdeps y --keep-going @world && emerge -v --depclean && revdep-rebuild -v && etc-update && eclean -d distfiles" && return
}
addtemplate world


# chroot: gentoo style
_gentoo-chroot() {
    [[ "$1" == "" ]] && echo "Example usage:\ngentoo-chroot /mnt/gentoo" && return 0
    [[ ! -d "$1"  ]] && echo "Your specified path $1 doesn't exist" && return 1
    sudo cp -L /etc/resolv.conf "$1/etc/resolv.conf"
    sudo mount -o bind /proc "$1/proc"
    sudo mount --rbind /dev  "$1/dev"
    sudo mount --rbind /sys  "$1/sys"
    sudo chroot "$1/" /bin/env -i TERM=$TERM /bin/bash
}
addtemplate gentoo-chroot chroot


_valgrind-memory-leak-check() {
    valgrind --tool=memcheck --leak-check=yes "$@"
}
addtemplate valgrind-memory-leak-check valgrind


# get public IP address
_ip() {
    dig +short myip.opendns.com @resolver1.opendns.com
}
addtemplate ip dig


# ping google.com
_pingg() {
    ping "${1:-google.com}"
}
addtemplate pingg ping


# makepkg: clean out build leftovers
_makepkg-clean() {
  echo "--> Cleaning up makepkg files..."
  makepkg -code

  echo "--> Removing any left out AUR tarballs or packages..."
  find . -name '*.src.tar.gz' -delete
  find . -name '*.pkg.tar' -delete
  find . -name '*.pkg.tar.xz' -delete
}
addtemplate makepkg-clean makepkg


# gitignore.io. Example usage: gi ruby,rails
_gi() {
    curl "http://www.gitignore.io/api/$@"
}
addtemplate gi curl


_git-clean() {
    git remote prune origin
}
addtemplate git-clean git


_gpg-encrypt-simm() {
    gpg -o "$1".gpg --symmetric "$1"
}
addtemplate gpg-encrypt-simm gpg


# youtube-dl: download video as mp3
_ytdl-mp3() {
    youtube-dl --continue --title --restrict-filenames --extract-audio --audio-format mp3 "$@"
}
addtemplate ytdl-mp3 youtube-dl

# youtube-dl: download video
_ytdl-video() {
    youtube-dl --continue --title --restrict-filenames "$@"
}
addtemplate ytdl-video youtube-dl


# pacman: given a specific full-path file, get a diff between he
# current one on the system and the one in its respective package
_pacman-diff-file() {
    if [[ "$1" == "" ]]; then
        cat <<EOF
          Purpose: get the changes between this file now and in the upstream package
          Usage: qkkdiff-file <file>
          Example: qkkdiff-file /etc/pacman.conf"
EOF
    elif [[ -f "$1" ]]; then
        pkg="$(pacman -Qo $1 | awk '//{printf "%s-%s", $(NF-1), $NF;}')"
        bsdtar -xOf /var/cache/pacman/pkg/${pkg}-$(uname -m).pkg.tar.xz "${1/\//}" | diff - "$1"
        return 0
    else
        echo "The provided file \e[0;31m${1}\e[0m does not exist."
        return 1
    fi
}
addtemplate pacman-diff-file pacman

# pacman: given a package name, get differences between the files it contain
# and the ones in the system
_pacman-diff-pkg() {
    if [[ "$1" == "" ]]; then
        echo <<EOF
          Purpose: get the changes between the upstream package and the files on the filesystem
          Usage: qkkdiff <package>
          Example: qkkdiff pacman
EOF
    elif ! pacman -Q "$1"; then
        return 1
    else
        ver=$(pacman -Q "$1" | cut -f2 -d' ')
        pacman -Qkkq "$1" | while read package file; do echo $file; bsdtar -xOf /var/cache/pacman/pkg/${package}-${ver}-$(uname -m).pkg.tar.xz ${file/\//} | diff - $file  ; done
        return 0
    fi
}
addtemplate pacman-diff-pkg pacman

# get extra packages installed on the pacman-based system
_pacman-extrapkgs() {
    comm -23 <(pacman -Qeq | sort) <(pacman -Qgq base base-devel | sort)
}
addtemplate pacman-extrapkgs pacman

# pacman: apt-get like build-dep
_pacman-build-dep() {
    sudo pacman -S $(expac -S "%E" "$@")
}
addtemplate pacman-build-dep pacman

# pacman: clean-up
_pacman-clean() {
    sudo pacman -Rnsc $(pacman -Qdtq)
    sudo paccache -r
}
addtemplate pacman-clean pacman


_ffmpeg-screencast-0() {
    [[ "$1" == "1" ]] && \
    ffmpeg -f alsa -i pulse -acodec pcm_s16le -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -r 25 -i :0.0 -qscale 0 ffmpeg-out-$(date '+%Y-%m-%d-%H-%M-%S').mkv || \
    ffmpeg -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -r 25 -i :0.0 -qscale 0 ffmpeg-out-$(date '+%Y-%m-%d-%H-%M-%S').mkv
}
_ffmpeg-screencast-1() {
    D="ffmpeg-out-$(date '+%Y-%m-%d-%H-%M-%S').mkv"
    [[ "$1" == "1" ]] && \
    ffmpeg -f alsa -i pulse -acodec pcm_s16le -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -i :0.0 -c:v libx264 -preset ultrafast -crf 0 "$D" || \
    ffmpeg -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -i :0.0 -c:v libx264 -preset ultrafast -crf 0 "$D"
}
_ffmpeg-screencast-2() {
    D="ffmpeg-out-$(date '+%Y-%m-%d-%H-%M-%S').avi"
    [[ "$1" == "1" ]] && \
    ffmpeg -f alsa -i pulse -acodec pcm_s16le -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -r 25 -i :0.0 -qscale 0 -vcodec huffyuv "$D" || \
    ffmpeg -f x11grab -s $(xdpyinfo | awk '/dimensions:/ {print $2}') -r 30 -i :0.0 -qscale 0 -vcodec huffyuv "$D"
    echo 'Compressing video (avi -> mkv)...'
    ffmpeg -i "$D" "${D/avi/mkv}" && rm "$D"
}
addtemplate ffmpeg-screencast-0 ffmpeg
addtemplate ffmpeg-screencast-1 ffmpeg
addtemplate ffmpeg-screencast-2 ffmpeg
