#!/bin/sh

# Inspiration from https://frantic.im/cdtmp/ and grml-zsh-config's cdt
# Usage: cdtmp [foo]
# Creates temp dir with user, date, and optional prefix
cdtmp() {
	builtin cd "$(mktemp -d -t "$USER-${1:+$1-}$(date +%Y-%m-%d)-XXXXXX")" || return
	builtin pwd
}
