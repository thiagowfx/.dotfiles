# Manage the dotfiles environment.

# List of default packages to install.
PACKAGES := \
	ack \
	bash \
	bin \
	fzf \
	git \
	profile \
	ranger \
	ssh \
	tmux \
	vim \
	zsh

DOTFILESDIR := $(shell dirname "$(readlink -f "$0")")
TARGETDIR := ~

# Abort if git is not installed.
ifeq (, $(shell which git))
  $(error "No git in $$PATH, install it first")
endif

# Abort if stow is not installed.
ifeq (, $(shell which stow))
  $(error "No stow in $$PATH, install it first")
endif

all: install lint

ansible:
	ansible-galaxy install -r requirements.yml
	ansible-playbook -K bootstrap.yml -i inventory.ini

ansible-lint:
	ansible-lint roles/

clean:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

install: stow ansible

lint: ansible-lint stow-lint

stow:
	stow $(PACKAGES)

stow-lint:
	chkstow -t $(TARGETDIR)  # find dangling symlinks

uninstall unstow:
	stow -D $(PACKAGES)

.PHONY: all clean install uninstall lint stow stow-lint unstow ansible ansible-lint
