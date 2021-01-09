# dotfiles

This repository is managed with GNU `stow`.

Symlink all dotfiles:

```
git clone --recurse-submodules "https://github.com/thiagowfx/.dotfiles" "$HOME/.dotfiles"
(cd "$HOME/.dotfiles" && stow --restow main corp)
```

Symlink all dotfiles *and* perform all installation steps (package management, etc):

```
git clone --recurse-submodules "https://github.com/thiagowfx/.dotfiles" "$HOME/.dotfiles"
"$HOME/.dotfiles/install.sh"
```
