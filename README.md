# Dotfiles

This repository is managed with GNU [`stow`][stow] and GNU [`make`][make].

[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/

## Workflow

### 1) Install all dotfiles

```
git clone --recurse https://github.com/thiagowfx/.dotfiles ~/.dotfiles && make -C ~/.dotfiles install
```

### 2) Update all dotfiles

```
make -C ~/.dotfiles update
```

## Notes

- `archived/` is not supported, it is preserved for historical purposes only.
