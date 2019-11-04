#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

for file in $files; do
    echo "Creating symlink for " + $file;
    ln -s $dir/$file ~/.$file;
done

# Other folders
mkdir ~/Text;
mkdir ~/School;

touch ~/Text/notes.org;
touch ~/Text/todo.org;
touch ~/Text/calendar.org;
