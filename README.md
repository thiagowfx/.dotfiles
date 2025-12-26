# thiagowfx's dotfiles

[![Pre-commit](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml)
[![Pre-commit autoupdate](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml)

This dotfiles repository is managed with [`GNU stow`][stow] and [`Just`][just].

```bash
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
(cd ~/.dotfiles && just install)
```

[just]: https://just.systems/
[stow]: https://www.gnu.org/software/stow/
