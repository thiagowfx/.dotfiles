#!/bin/bash

# homebrew
hash brew >/dev/null 2>&1 && src_files "$(brew --prefix)/etc/profile.d/bash_completion.sh"

# unclutter your .profile: load/unload env depending on the current directory
# https://direnv.net/
hash direnv >/dev/null 2>&1 && eval "$(direnv hook bash)"

# github's official command line tool
hash gh >/dev/null 2>&1 && eval "$(gh completion -s bash)"

# a smarter cd command
# https://github.com/ajeetdsouza/zoxide
hash zoxide >/dev/null 2>&1 && eval "$(zoxide init bash)"

# pytest.org
hash register-python-argcomplete && hash pytest && eval "$(register-python-argcomplete pytest)"
