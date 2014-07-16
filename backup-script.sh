# -*- sh -*-

ORIGIN_DIRS=(
    "$HOME/Apps"
    "$HOME/Downloads"
    "$HOME/Dropbox"
    "$HOME/Music"
    "$HOME/Videos"
)

PREFIX=""

TARGET_DIR=(
    "$PREFIX/ideapad/backup"
)

for folder in "${ORIGIN_DIRS[@]}"; do
    echo "$folder" "$TARGET_DIR/$folder"
done
