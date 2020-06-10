#!/bin/sh

sh .xprofile;
dunst &
systemctl start --user emacs &
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &

picom &
mpd &
sxhkd &

$HOME/.config/bspwm/bspwmrc &
polybar custom-bar &
