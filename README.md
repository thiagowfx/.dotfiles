# Dotfiles

This repository is managed with GNU [`stow`][1].

## Workflow

### 1) Install all dotfiles

```
git clone --recurse https://github.com/thiagowfx/.dotfiles && .dotfiles/setup.sh --install
```

### 2) Update all dotfiles

```
.dotfiles/setup.sh --update
```

[1]: https://www.gnu.org/software/stow/

## `archived/`

Preserved for historical purposes, providing sensible defaults.

### GNU screen

- Prefer tmux over GNU screen.
