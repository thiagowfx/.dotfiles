# Manage the dotfiles environment.
#
# Packages not included:
#   misc/
#   vendor/

# List of packages to install.
PACKAGES := \
	ack \
	alacritty \
	alpine \
	arch \
	bash \
	brew \
	chromium \
	fzf \
	git \
	hg \
	i3 \
	ranger \
	rofi \
	screen \
	shell \
	ssh \
	sway \
	tmux \
	tmux_auto_ssh \
	vim \
	vscode \
	x11 \
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

install: $(PACKAGES)

uninstall:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(PACKAGES)

lint:
	# Find dangling symlinks
	chkstow -t $(TARGETDIR)

define stowrule
$(1):
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --restow $(1)
endef
$(foreach package,$(PACKAGES),$(eval $(call stowrule,$(package))))

.PHONY: all install uninstall lint $(PACKAGES)
