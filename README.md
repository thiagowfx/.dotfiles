# Dotfiles

This repository is managed with GNU [`stow`][stow] and GNU [`make`][make].

[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/

## Workflow

### 1) Install

```
git clone --recurse https://github.com/thiagowfx/.dotfiles ~/.dotfiles
make -C ~/.dotfiles install
```

### 2) Update

```
make -C ~/.dotfiles update
```
