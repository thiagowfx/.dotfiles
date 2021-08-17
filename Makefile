# Manage the dotfiles environment.

DOTFILESDIR := $(shell dirname "$(readlink -f "$0")")
MODULES := main

ifeq (, $(shell which stow))
  $(error "No stow in $(PATH), run apt install stow first")
endif

all: install update

install: submodules-install stow tmux-install vim-install

uninstall: unstow

update: submodules-update stow tmux-update vim-update

clean: tmux-clean vim-clean

stow:
	stow -t ~ -d $(DOTFILESDIR) --restow $(MODULES)

unstow:
	stow -t ~ -d $(DOTFILESDIR) --delete $(MODULES)

submodules-install:
	git -C $(DOTFILESDIR) submodule update --init

submodules-update: submodules-install
	git -C $(DOTFILESDIR) submodule update --remote

tmux-clean:
	~/.tmux/plugins/tpm/bin/clean_plugins

tmux-install:
	~/.tmux/plugins/tpm/bin/install_plugins

tmux-update: tmux-install
	~/.tmux/plugins/tpm/bin/update_plugins all

vim-clean:
	vim +PlugClean! +qall

vim-install:
	vim +PlugInstall +qall

vim-update: vim-install
	vim +PlugUpgrade +PlugUpdate +qall

.NOTPARALLEL:
.PHONY: all install uninstall update clean
