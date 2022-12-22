# Manage the dotfiles environment.

# List of all valid packages.
#
# Packages not included:
#   misc/
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
	ranger \
	rofi \
	screen \
	scripts \
	shell \
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
	ranger \
	scripts \
	shell \
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

all: install lint

install: $(DEFAULT_PACKAGES)

uninstall:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

lint:
	# Find dangling symlinks
	chkstow -t $(TARGETDIR)

define stowrule
$(1):
	stow --no-folding -t $(TARGETDIR) -d $(DOTFILESDIR) --restow $(1)
endef
$(foreach package,$(PACKAGES),$(eval $(call stowrule,$(package))))

.PHONY: all install uninstall lint $(PACKAGES)
