#!/bin/bash

dir=~/Dotfiles;
files=`cat $dir/files`;

for file in $files; do
    echo "Creating symlink for " + $file;
    ln -s $dir/$file ~/.$file;
done

# Other folders
mkdir ~/Text;
mkdir ~/Programming;
mkdir ~/Projects;
mkdir ~/Projects/Templates;
mkdir ~/School;

touch ~/Text/notes.org;
touch ~/Text/todo.org;
touch ~/Text/calendar.org;

declare -a templates=("CTemplate" "CPPTemplate" "PythonTemplate" "NodeTemplate" "ArduinoTemplate");
for template in ${templates[@]}; do
    git clone https://github.com/Oreodave/$template ~/Projects/Templates/$template;
done
