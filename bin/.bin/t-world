#!/bin/bash
# emerge an unix system

# exit immediately upon receiving C-c
trap ctrl_c INT
function ctrl_c {
	exit 1
}

# ask for sudo password upfront
sudo -v

# vim-plug
if [[ -d "$HOME/.vim/vim-plug" ]] && hash vim &>/dev/null; then
	vim +PlugInstall +PlugClean! +qall
fi

# dotfiles
if [[ -d "$HOME/.dotfiles" ]]; then
	(cd "$HOME/.dotfiles" && git pull --recurse-submodules)
fi

# macOS
if [[ "$(uname)" == "Darwin" ]]; then
	sudo softwareupdate -i -a

	# homebrew
	if command -v brew &>/dev/null; then
		brew update
		brew upgrade
		brew cleanup
		brew prune
		brew doctor

		# casks upgrade
		brew cu --cleanup --yes

		# homebrew backup: updates ~/.Brewfile
		brew bundle dump --force --global
	fi

	# macports
	if command -v port &>/dev/null; then
		sudo port selfupdate
		sudo port upgrade outdated
		sudo port uninstall leaves
	fi

	# mac app store cmdline
	if command -v mas &>/dev/null; then
		mas upgrade
	fi

	exit
fi

# FreeBSD
if [[ "$(uname)" == "FreeBSD" ]] && command -v pkg &>/dev/null; then
	sudo pkg update
	sudo pkg upgrade

	exit
fi

# Arch Linux
if [[ -e /etc/arch-release ]] && command -v pacman &>/dev/null; then
	sudo pacman -Syyu
	sudo pacman -Rnsc $(pacman -Qdtq) # remove leaves
	sudo paccache -r

	exit
fi

# Gentoo Linux
if [[ -e /etc/gentoo-release ]] && command -v emerge &>/dev/null; then
	sudo emerge --sync
	sudo emerge -auDN --with-bdeps y --keep-going @world
	sudo emerge -v --depclean
	sudo revdep-rebuild -v
	sudo etc-update
	sudo eclean -d distfiles

	exit
fi

# Apt-based Linux (Debian / Ubuntu)
if [[ -e /etc/debian_version ]] && command -v apt &>/dev/null; then
	sudo apt update
	sudo apt upgrade
	sudo apt-get dist-upgrade
	sudo apt-get autoremove
	sudo apt-get clean
	command -v checkrestart &>/dev/null && sudo checkrestart

	exit
fi

# One pseudo package manager to rule them all
if command -v pacapt &>/dev/null; then
	pacapt -Syu
	pacapt -Sccc

	exit
fi
