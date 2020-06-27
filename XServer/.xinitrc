#!/bin/sh

sh .xprofile;
killall mpd;
killall picom;
killall sxhkd;
xset s 1800;

$HOME/.local/scripts/eserver start &
dunst &
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &

picom &
mpd &
sxhkd &

$HOME/.config/bspwm/bspwmrc
