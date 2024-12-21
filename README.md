# thiagowfx's dotfiles

[![Pre-commit](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml)
[![Pre-commit autoupdate](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml)

This dotfiles repository is managed with [`GNU stow`][stow], [`GNU make`][make] and [`Ansible`][ansible].

```
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
make -C ~/.dotfiles
```

[ansible]: https://www.ansible.com/
[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/
