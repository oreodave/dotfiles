#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --auto --output HDMI1 --primary;
xrandr --auto --output eDP1 --right-of HDMI1;
xrandr --auto --output VGA1 --primary --left-of DP2;
xrandr --auto --output DP2 left;

$(xss-lock --transfer-sleep-lock -- $HOME/.local/scripts/lock) &
$HOME/.local/scripts/background &

systemctl --user start emacs &
$HOME/.local/scripts/status/music_update_bar &
dunst &
picom &
sxhkd &
dwmblocks &

# $HOME/.config/bspwm/bspwmrc
while :; do
  dwm
done
