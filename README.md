# thiagowfx's dotfiles

## CI status

```shell
grep -Erl '\b(push|schedule|workflow_dispatch):$' .github/workflows | \
  xargs -n 1 basename | sort -d | \
  sed -e 's|^[^-].*|- [![](https://github.com/thiagowfx/.dotfiles/actions/workflows/&/badge.svg?branch=master)](https://github.com/thiagowfx/.dotfiles/actions/workflows/&)|'
```

<!-- BEGIN mdsh -->
- [![pre-commit-autoupdate](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml/badge.svg?branch=master)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yml)
- [![pre-commit](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml/badge.svg?branch=master)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yml)
<!-- END mdsh -->

This dotfiles repository is managed with [`GNU stow`][stow] and [`Just`][just].

```bash
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
(cd ~/.dotfiles && just install)
```

[just]: https://just.systems/
[stow]: https://www.gnu.org/software/stow/
