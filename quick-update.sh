#!/bin/sh
#
# Quickly update / upgrade all dotfiles.
#

git pull --recurse-submodules
git submodule update --recursive --remote
