#!/bin/zsh
# Miscellaneous functions.

# Monitor a folder and rsync it in case of updates using inotify
# Note: Need inotify-tools package.
rsync_watch() {
  rsync --copy-links --progress --recursive "$1" "$2"
  while inotifywait -r -e create,delete,modify "$1"; do
    rsync --copy-links --progress --recursive "$1" "$2"
  done
}
