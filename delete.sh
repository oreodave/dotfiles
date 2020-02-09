#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

for file in $files; do
    echo "Removing symlink for ~/." + $file;
    rm ~/.$file;
done

rm -rf ~/Text ~/Code ~/Code/Learning ~/Code/Projects \
    ~/Code/Templates ~/School;

rm -rf ~/.emacs.d;
