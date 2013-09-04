#!/bin/bash
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
# Based on http://pastebin.com/kwVN3QQH

########################################
# variables - customize here
########################################

dir=~/git/dotfiles		# dotfiles directory
olddir=~/.dotfiles_backup	# old dotfiles backup directory
vundledir=~/.vim/bundle/vundle
files=".aliases .bashrc .conkyrc .emacs .gitconfig .gitignore .vimrc .yaourtrc .zshrc" # list of files/folders to symlink in 
homedir
#musicdir=~/Copy/Music
#picturesdir=~/Dropbox/Pictures
#videosdir=~/Downloads/Torrents
#templatesdir=~/Copy/Templates

########################################
# create custom links for dotfiles
########################################

# create dotfiles_backup in homedir
mkdir -p $olddir

# change to the dotfiles directory
cd $dir

# move any existing dotfiles in homedir to dotfiles_backup directory, then create symlinks 
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/$file $olddir
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/$file
done

########################################
# create symlinks for folders
########################################
# if [[ -f $musicdir ]]; then
#     touch .dummy
# else
#     mkdir -p $musicdir
#     ln -s $musicdir ~/Music
# fi
# 
# if [[ -f $picturesdir ]]; then
#     touch .dummy
# else
#     mkdir -p $picturesdir
#     ln -s $picturesdir ~/Pictures
# fi
# 
# if [[ -f $videosdir ]]; then
#     touch .dummy
# else
#     mkdir -p $videosdir
#     ln -s $videosdir ~/Videos
# fi
# 
# if [[ -f $templatesdir ]]; then
#     touch .dummy
# else
#     mkdir -p $templatesdir
#     ln -s $templatesdir ~/Templates
# fi
# 
