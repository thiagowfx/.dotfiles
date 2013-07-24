#!/bin/bash
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
# Based on http://pastebin.com/kwVN3QQH

########################################
# Variables - customize here
########################################

dir       = ~/dotfiles		# dotfiles directory
olddir    = ~/.dotfiles_backup	# old dotfiles backup directory
vundledir = ~/.vim/bundle/vundle
files     = ".bashrc .conkyrc .emacs .gitconfig .gitignore .vimrc .yaourtrc .zshrc" # list of files/folders to symlink in homedir

########################################

# create dotfiles_old in homedir
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# change to the dotfiles directory
echo "Changing to the $dir directory"
cd $dir
echo "...done"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks 
for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/$file ~/dotfiles_old/
    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/$file
done
