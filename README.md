# dotfiles

> Do one thing, and do it well...

Managed with GNU stow. **Quick setup**:

    git clone --depth=1 https://github.com/thiagowfx/.dotfiles $HOME/.dotfiles

To **stow**:

    cd $HOME/.dotfiles && stow bash git [... other directories ...]

To **unstow**:

    cd $HOME/.dotfiles && stow -D bash git [... other directories ...]
