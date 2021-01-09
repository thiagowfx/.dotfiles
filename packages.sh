#!/bin/bash
#
# Install sensible packages.

set -xeuo pipefail

readonly PACKAGES=(
  # cli
  ack
  bash
  bash-completion
  fzf
  git
  highlight
  less
  ranger
  shellcheck
  ssh
  stow
  tig
  tmux
  trash-cli
  tree
  vim

  # graphical
  dex
  diodon
  feh
  i3
  i3status
  redshift
  rofi
  tilix
  unclutter
  x11-xserver-utils
  xscreensaver
  xss-lock
)

sudo apt install "${PACKAGES[@]}"
