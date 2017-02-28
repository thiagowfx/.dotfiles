dotfiles
========

"Do one thing, and do it well" **dotfiles**.

Managed with GNU stow. Quick **stow**:

    cd $HOME
    git clone https://github.com/thiagowfx/dotfiles .dotfiles
    cd .dotfiles
    stow bash git tmux [... other directories ... ]

To **unstow**:

    cd $HOME/.dotfiles
    stow -D bash git tmux [... other directories ...]
