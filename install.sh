#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

for file in $files; do
    echo "Creating symlink for " + $file;
    ln -s $dir/$file ~/.$file;
done
