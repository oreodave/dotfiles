#!/bin/bash

folders=`find . -maxdepth 1 -type 'd' -not -name '.git' -not -name '.'`;

# Symlink profiles
for pkg in $folders; do
	echo "Stowing " $pkg;
	stow $pkg
done

# Generate folders
mkdir ~/Text;
mkdir ~/Code;
mkdir ~/Code/Learning;
mkdir ~/Code/Projects;
mkdir ~/Code/Templates;
mkdir ~/School;

# get templates
declare -a templates=("CTemplate" "CPPTemplate" "PythonTemplate"
					  "NodeTemplate" "ArduinoTemplate");
for template in ${templates[@]}; do
	git clone https://github.com/odavep/$template ~/Code/Templates/$template;
done
