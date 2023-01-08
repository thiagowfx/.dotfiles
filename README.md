# thiagowfx's dotfiles

This dotfiles repository is managed with [`GNU stow`][stow] and [`GNU make`][make]:

```
git clone --recurse --jobs=$(nproc) https://github.com/thiagowfx/.dotfiles ~/.dotfiles
make -C ~/.dotfiles
```

[make]: https://www.gnu.org/software/make/
[stow]: https://www.gnu.org/software/stow/

## software ([uses](https://uses.tech/))

- OS: arch linux, alpine linux, debian
- terminal emulator: alacritty, hterm, iterm2
- shell: bash, zsh + grml-zsh-config
- web browser: chromium
- window manager: i3, sway
- terminal multiplexer: tmux
- text editor: vim, vscode
