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

all: ansible install

clean:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

install:
	stow $(PACKAGES)

lint:
	# Find dangling symlinks
	chkstow -t $(TARGETDIR)

ansible:
	ansible-galaxy install -r requirements.yml
	ansible-playbook bootstrap.yml -i inventory.ini

.PHONY: all clean install lint ansible
