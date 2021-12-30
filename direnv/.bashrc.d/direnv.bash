#!/bin/bash
# https://direnv.net/
if hash direnv >/dev/null 2>&1; then
	eval "$(direnv hook bash)"
fi
