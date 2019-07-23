#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

for file in $files; do
    echo "Removing symlink for ~/." + $file;
    rm ~/.$file;
done
