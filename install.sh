#!/bin/bash

files="spacemacs vimrc zshenv tmux";
dir=~/Dotfiles;

for file in $files; do
    echo "Creating symlink for " + $file;
    ln -s $dir/$file .$file;
done
