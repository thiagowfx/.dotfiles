#!/bin/bash

DOTFILES_DIR="$HOME/.dotfiles"

GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NO_COLOR='\033[0m'

log_warning() {
  (>&2 echo -e "${YELLOW}WARNING: $@${NO_COLOR}")
}

log_completed() {
  (echo -e "${GREEN}COMPLETED: $@${NO_COLOR}")
}

do_i3() {
  ! hash j4-make-config  &>/dev/null && log_warning "j4-make-config is not installed" && return
  ! hash i3-msg &>/dev/null && log_warning "i3 is not installed" && return

  j4-make-config
  i3-msg restart

  log_completed "i3"
}

do_ranger() {
  ! hash ranger &>/dev/null && log_warning "ranger is not installed" && return

  ranger --copy-config=scope

  log_completed "ranger"
}

# ASSUMES: git is installed, dotfiles were recursively cloned
do_submodules() {
  git -C "${DOTFILES_DIR}" pull --recurse-submodules

  log_completed "submodules"
}

do_stow() {
  local -r STOW_DIRS=$(cd "${DOTFILES_DIR}"; ls -d */ | cut -f1 -d'/' | grep -v '.git')

  ! hash stow &>/dev/null && log_warning "stow is not installed" && return

  for dir in ${STOW_DIRS[*]}; do
    stow -d "${DOTFILES_DIR}" --restow "${dir}"
  done

  log_completed "restow"
}

do_vim_plug() {
  [[ ! -d "$HOME/.vim/vim-plug" ]] && log_warning "vim-plug directory doesn't exist" && return
  ! hash vim &>/dev/null && log_warning "vim is not installed" && return

  vim +PlugInstall +PlugClean! +qall

  log_completed "vim-plug"
}

# recurring; order is important
do_submodules; do_stow; do_i3; do_vim_plug

# one-time
do_ranger
