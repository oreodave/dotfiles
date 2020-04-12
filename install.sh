#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

# Symlink profiles
for file in $files; do
    echo "Creating symlink for " + $file;
    ln -s $dir/$file ~/.$file;
done

## Symlink to .config
ln -s $dir/rofi.conf ~/.config/rofi/config

# Generate folders
mkdir ~/Text;
mkdir ~/Code;
mkdir ~/Code/Learning;
mkdir ~/Code/Projects;
mkdir ~/Code/Templates;
mkdir ~/School;

touch ~/Text/notes.org;
touch ~/Text/todo.org;
touch ~/Text/calendar.org;

# get templates
declare -a templates=("CTemplate" "CPPTemplate" "PythonTemplate"
                      "NodeTemplate" "ArduinoTemplate");
for template in ${templates[@]}; do
    git clone https://github.com/Oreodave/$template ~/Code/Templates/$template;
done

# get doom emacs
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install;
~/.emacs.d/bin/doom sync;
