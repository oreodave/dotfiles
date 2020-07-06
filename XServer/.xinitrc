#!/bin/sh

killall mpd;
killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &
$HOME/.local/scripts/eserver start &
dunst &
picom &
mpd &
sxhkd &
dwmblocks &
# $HOME/.config/bspwm/bspwmrc
while :; do
    ssh-agent dwm
done
