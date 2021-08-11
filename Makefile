# Manage the dotfiles environment.

DOTFILESDIR := $(shell dirname "$(readlink -f "$0")")

stow:
	stow -t ~ -d $(DOTFILESDIR) --restow main

submodules-install:
	git -C $(DOTFILESDIR) submodule update --init

submodules-update:
	git -C $(DOTFILESDIR) submodule update --remote

tmux-install:
	~/.tmux/plugins/tpm/bin/install_plugins

tmux-update:
	~/.tmux/plugins/tpm/bin/update_plugins all

vim-install:
	vim +PlugClean! +PlugInstall +qall

vim-update:
	vim +PlugClean! +PlugUpgrade +PlugUpdate +qall

install: submodules-install stow tmux-install vim-install

update: submodules-update stow tmux-update vim-update

help:
	@echo "make [install | update]"
