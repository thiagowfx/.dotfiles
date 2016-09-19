# -*- shell-script -*-
# 
# ASSUMES: sourced from a shell where source_if_exists has been defined.
#

if [[ $TERM == xterm-termite ]]; then
      source_if_exists "/etc/profile.d/vte.sh"
fi

# autojump
command -v brew &>/dev/null && source_if_exists "$(brew --prefix)/etc/profile.d/autojump.sh"

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
    addenv PATH "$1:$PATH"
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

addalias g "git" git

addalias ls "ls -F" ls
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
addpath "/usr/lib/ccache/bin"
addpath "/opt/local/bin"
addpath "/opt/local/sbin"
addpath "/usr/local/sbin"
command -v ruby &>/dev/null && addpath "$(ruby -rubygems -e "puts Gem.user_dir")/bin"

addwine WINEARCH "win32"
addwine WINEDEBUG "fixme-all"
addwine WINEDLLOVERRIDES "winemenubuilder.exe=d"


# upgrade an Unix / Linux system
_world() {
    [[ "$(uname)" == "Darwin" ]] && command -v brew &>/dev/null && brew update && brew upgrade --all && brew cleanup && brew prune && brew doctor && return
    [[ "$(uname)" == "Darwin" ]] && command -v port &>/dev/null && sudo port selfupdate && sudo port upgrade outdated && return

    [[ -e /etc/arch-release ]] && sudo pacman -Syu "$@" && sudo pacman -Rnsc $(pacman -Qdtq) && sudo paccache -r && return
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
