# Dotfiles

This repository is managed with GNU [`stow`][stow].

[stow]: https://www.gnu.org/software/stow/

## Workflow

### 1) Install all dotfiles

```
git clone --recurse https://github.com/thiagowfx/.dotfiles ~/.dotfiles && ~/.dotfiles/setup.sh --install
```

### 2) Update all dotfiles

```
~/.dotfiles/setup.sh --update
```

Note: `archived/` is preserved for historical purposes.
