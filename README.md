dotfiles
========

"Do one thing, and do it well" **dotfiles**.

Managed with GNU stow. **Quick setup**:

    cd $HOME
    git clone --depth=1 https://github.com/thiagowfx/dotfiles .dotfiles
    cd .dotfiles

To **stow**:

    stow bash git [... other directories ... ]

To **unstow**:

    cd $HOME/.dotfiles
    stow -D bash git [... other directories ...]
