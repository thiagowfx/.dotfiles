# thiagowfx's dotfiles

This dotfiles repository is managed with [`GNU stow`][stow], [`GNU make`][make] and [`Ansible`][ansible]

```
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
make -C ~/.dotfiles
```

[ansible]: https://www.ansible.com/
[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/

## ansible

- `ansible.cfg`: config
- `bootstrap.yml`: tasks
- `inventory`: hosts
- `requirements.yml`: dependencies

## software ([uses](https://uses.tech/))

- OS: arch linux, alpine linux, debian
- terminal emulator: alacritty (Linux), hterm (ChromeOS), iterm2 (macOS)
- shell: bash, zsh + grml-zsh-config
- web browser: chromium (Linux), google chrome
- window manager: i3 (X11), sway (Wayland)
- terminal multiplexer: tmux
- text editor: vim, vscode
