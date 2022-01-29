#!/usr/bin/env sh

git clone --depth 1 --branch emacs-27.2 master git://git.sv.gnu.org/emacs.git .;

./configure --with-xwidgets -with-threads --with-harfbuzz --with-jpeg \
            --with-json --with-modules;
make;
sudo make install;
