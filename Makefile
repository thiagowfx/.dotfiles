# Manage the dotfiles environment.

# List of modules to install.
MODULES := alacritty bash fzf git hg i3 ranger screen shell ssh systemd tmux vim x11 zsh

# Abort if git is not installed.
ifeq (, $(shell which git))
  $(error "No git in $$PATH, install it first")
endif

# Abort if stow is not installed.
ifeq (, $(shell which stow))
  $(error "No stow in $$PATH, install it first")
endif

DOTFILESDIR := $(shell dirname "$(readlink -f "$0")")

all: install update

install: submodules stow tmux vim

update: stow tmux-update vim-update

clean: unstow tmux-clean vim-clean

archlinux:
	makepkg -sirc

archlinux-clean:
	sudo pacman -Rnsc thiagowfx-dotfiles-meta

stow:
	stow -t ~ -d $(DOTFILESDIR) --restow $(MODULES)

unstow:
	stow -t ~ -d $(DOTFILESDIR) --delete $(MODULES)

submodules:
	git -C $(DOTFILESDIR) submodule update --init

pull: submodules
	git -C $(DOTFILESDIR) submodule update --remote

tmux-clean:
	~/.tmux/plugins/tpm/bin/clean_plugins

tmux:
	~/.tmux/plugins/tpm/bin/install_plugins

tmux-update: tmux
	~/.tmux/plugins/tpm/bin/update_plugins all

vim-clean:
	vim +PlugClean! +qall

vim:
	vim +PlugInstall +qall

vim-update: vim
	vim +PlugUpgrade +PlugUpdate +qall

.NOTPARALLEL:
.PHONY: all install update clean archlinux archlinux-clean stow unstow submodules pull tmux-clean tmux tmux-update vim-clean vim vim-update
