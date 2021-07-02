# Dotfiles

This repository is managed with GNU [`stow`][1].

## Workflow

### 1) Install all dotfiles

```
git clone https://github.com/thiagowfx/.dotfiles && .dotfiles/install.sh
```

### 2) Update all submodules

```
git submodule update --recursive --remote
```

[1]: https://www.gnu.org/software/stow/
