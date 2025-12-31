# thiagowfx's dotfiles

## CI status

<!-- markdownlint-disable MD013 MD045 -->
<!--
> $ grep -Erl '\b(push|schedule|workflow_dispatch):$' .github/workflows | xargs -n 1 basename | sort -d | sed -e 's|^[^-].*|- [![](https://github.com/thiagowfx/.dotfiles/actions/workflows/&/badge.svg?branch=master)](https://github.com/thiagowfx/.dotfiles/actions/workflows/&)|'
-->

<!-- BEGIN mdsh -->
<!-- END mdsh -->
<!-- markdownlint-enable MD013 MD045 -->

This dotfiles repository is managed with [`GNU stow`][stow] and [`Just`][just].

```bash
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
(cd ~/.dotfiles && just install)
```

[just]: https://just.systems/
[stow]: https://www.gnu.org/software/stow/
