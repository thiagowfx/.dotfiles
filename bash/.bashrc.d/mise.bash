#!/bin/bash

# mise: tool versions, env vars and tasks
# https://mise.jdx.dev/dev-tools/shims.html#how-to-add-mise-shims-to-path
# PATH activation for interactive shells; .profile.d/mise.sh sets up the
# shims that non-interactive shells use.
command -v mise >/dev/null 2>&1 && eval "$(mise activate bash)"
