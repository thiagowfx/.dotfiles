# Manage the dotfiles environment.
#
# Packages not included:
#   misc/
#   vendor/

# List of packages to install.
PACKAGES := \
	alacritty \
	apk \
	arch \
	bash \
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
	systemd \
	x11 \
	zsh

# These packages have their own targets.
ADVANCED_PACKAGES := \
		tmux \
		vim

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
	find $(TARGETDIR) -xtype l -delete -maxdepth 3

pull:
	git -C $(DOTFILESDIR) submodule update --init --remote

$(foreach package,$(PACKAGES),$(eval $(call stowrule,$(package))))

uninstall:
	stow -t $(TARGETDIR) -d $(DOTFILESDIR) --delete $(ALL_PACKAGES)

tmux:
	$(call stow,$@)
	~/.tmux/plugins/tpm/bin/install_plugins
	~/.tmux/plugins/tpm/bin/update_plugins all
	~/.tmux/plugins/tpm/bin/clean_plugins

vim:
	$(call stow,$@)
	vim +PlugInstall +qall
	vim +PlugUpgrade +PlugUpdate +qall
	vim +PlugClean! +qall

.PHONY: all install pull uninstall $(ALL_PACKAGES)
