# Manage the dotfiles environment.
#
# Packages not included:
#   misc/
#   vendor/

# List of packages to install.
PACKAGES := \
	alacritty \
	alpine \
	arch \
	bash \
	chromium \
	direnv \
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
	systemd \
	tmux \
	tmux_auto_ssh \
	vscode \
	x11 \
	zsh

# These packages have their own targets.
ADVANCED_PACKAGES := vim

ALL_PACKAGES := $(PACKAGES) $(ADVANCED_PACKAGES)

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

define stow
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --restow $(1)
endef

define stowrule
$(1):
	$(call stow,$(1))
endef

all: install

install: $(ALL_PACKAGES)
	# Delete dangling symlinks
	find $(TARGETDIR) -maxdepth 3 -xtype l -delete

pull:
	git -C $(DOTFILESDIR) submodule update --init --remote

uninstall:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(ALL_PACKAGES)

$(foreach package,$(PACKAGES),$(eval $(call stowrule,$(package))))

vim:
	$(call stow,$@)
	vim +PlugClean! +PlugInstall +PlugUpgrade +PlugUpdate +qall

.PHONY: all install pull uninstall $(ALL_PACKAGES)
