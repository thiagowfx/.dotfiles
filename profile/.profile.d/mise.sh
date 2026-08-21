#!/bin/sh

# mise: tool versions, env vars and tasks: https://mise.jdx.dev/
# Shims cover non-interactive shells (scripts, editors, IDEs). Interactive
# shells additionally get full PATH activation from .zshrc.d and .bashrc.d.
# path_munge rather than `mise activate --shims`, which emits the same export
# but unconditionally, so nested interactive shells accumulate duplicates.
path_munge "$HOME/.local/share/mise/shims"
