#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --auto --output HDMI1 --primary;
xrandr --auto --output eDP1  --right-of HDMI1;
xrandr --auto --output VGA1  --primary --mode 1920x1080;
xrandr --auto --output DP1   --left-of VGA1 --mode 1280x1024;

$(xss-lock --transfer-sleep-lock -- $HOME/.local/scripts/lock) &
$HOME/.local/scripts/background &

dunst &
picom --backend xrender --vsync --no-fading-openclose &
sxhkd &
dwmblocks &

# $HOME/.config/bspwm/bspwmrc
while :; do
  dwm
done
