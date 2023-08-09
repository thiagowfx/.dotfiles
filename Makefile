# Manage the dotfiles environment.

# List of all valid packages.
#
# Packages not included:
#   misc/
#   roles/
#   vendor/
PACKAGES := \
	ack \
	alacritty \
	alpine \
	arch \
	bash \
	chromium \
	fzf \
	git \
	hg \
	i3 \
	mr \
	profile \
	ranger \
	rofi \
	screen \
	scripts \
	ssh \
	sway \
	tmux \
	tmux_auto_ssh \
	vim \
	x11 \
	zsh

# List of default packages to install.
#
# Packages not included:
#   alacritty
#   alpine
#   arch
#   chromium
#   i3
#   rofi
#   screen
#   sway
#   x11
DEFAULT_PACKAGES := \
	ack \
	bash \
	fzf \
	git \
	hg \
	mr \
	profile \
	ranger \
	scripts \
	ssh \
	tmux \
	tmux_auto_ssh \
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

all: ansible stow

install: $(DEFAULT_PACKAGES)

uninstall:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

stow: $(DEFAULT_PACKAGES) stow-lint

stow-lint:
	# Find dangling symlinks
	chkstow -t $(TARGETDIR)

define stowrule
$(1):
	stow --no-folding -t $(TARGETDIR) -d $(DOTFILESDIR) --restow $(1)
endef
$(foreach package,$(PACKAGES),$(eval $(call stowrule,$(package))))

ansible: ansible-galaxy ansible-playbook

ansible-galaxy:
	ansible-galaxy install -r requirements.yml

ansible-playbook: ansible-galaxy
	ansible-playbook bootstrap.yml -i inventory

.PHONY: all install uninstall stow stow-lint ansible ansible-galaxy ansible-playbook $(PACKAGES)
