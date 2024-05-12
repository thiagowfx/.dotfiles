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

all: stow ansible

ansible:
	ansible-galaxy install -r requirements.yml
	ansible-playbook bootstrap.yml -i inventory.ini --tags untagged

stow:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) $(PACKAGES)

stow-lint:
	chkstow -t $(TARGETDIR)  # find dangling symlinks

unstow:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

.PHONY: all ansible stow stow-lint unstow
