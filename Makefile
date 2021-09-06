# Manage the dotfiles environment.

# List of modules to install.
MODULES := main screen

# Abort if stow is not installed.
ifeq (, $(shell which stow))
  $(error "No stow in $$PATH, install stow first")
endif

DOTFILESDIR := $(shell dirname "$(readlink -f "$0")")

all: install update

install: submodules stow tmux vim

uninstall: unstow

update: submodules-update stow tmux-update vim-update

clean: tmux-clean vim-clean

stow:
	stow -t ~ -d $(DOTFILESDIR) --restow $(MODULES)

unstow:
	stow -t ~ -d $(DOTFILESDIR) --delete $(MODULES)

submodules:
	git -C $(DOTFILESDIR) submodule update --init

submodules-update: submodules
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
.PHONY: all install uninstall update clean stow unstow submodules submodules-update tmux-clean tmux tmux-update vim-clean vim vim-update
