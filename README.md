# thiagowfx's dotfiles

## CI status

`> grep -Erl '\b(push|schedule|workflow_dispatch):$' .github/workflows | xargs -n 1 basename | sort -d | sed 's|^[^/]*$|[![&](https://github.com/thiagowfx/.dotfiles/actions/workflows/&/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/&)|'`

<!-- BEGIN mdsh -->
[![ls-lint.yml](https://github.com/thiagowfx/.dotfiles/actions/workflows/ls-lint.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/ls-lint.yml)
[![prek-autoupdate.yml](https://github.com/thiagowfx/.dotfiles/actions/workflows/prek-autoupdate.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/prek-autoupdate.yml)
[![prek.yml](https://github.com/thiagowfx/.dotfiles/actions/workflows/prek.yml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/prek.yml)
<!-- END mdsh -->

This dotfiles repository is managed with [`GNU stow`][stow] and [`Just`][just].

```bash
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
(cd ~/.dotfiles && just install)
```

[just]: https://just.systems/
[stow]: https://www.gnu.org/software/stow/
