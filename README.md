# thiagowfx's dotfiles

[![Pre-commit](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yaml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit.yaml)
[![Pre-commit autoupdate](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yaml/badge.svg)](https://github.com/thiagowfx/.dotfiles/actions/workflows/pre-commit-autoupdate.yaml)

This dotfiles repository is managed with [`GNU stow`][stow], [`GNU make`][make] and [`Ansible`][ansible].

```
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
make -C ~/.dotfiles
```

[ansible]: https://www.ansible.com/
[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/

## software ([uses](https://uses.tech/))

- OS: arch linux, alpine linux, debian, macOS
- terminal emulator: alacritty (Linux), hterm (ChromeOS), iterm2 (macOS)
- shell: bash, zsh with grml-zsh-config
- web browser: chromium (Linux), safari / google chrome (macOS)
- window manager: i3 (X11), sway (Wayland)
- terminal multiplexer: tmux
- text editor: vim (terminal), zed (lightweight editor), vscode (heavyweight IDE)
