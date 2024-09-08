#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --auto --output eDP1 --primary;
xrandr --auto --output HDMI1 --left-of eDP1 --scale 1.25x1.25;
xrandr --auto --output DP1   --left-of VGA1 --mode 1280x1024;
xrandr --auto --output VGA1  --primary --mode 1920x1080;

$(xss-lock --transfer-sleep-lock -- $HOME/.local/scripts/lock) &
$HOME/.local/scripts/background &

dunst &
picom --backend xrender --no-fading-openclose &
sxhkd &
dwmblocks &

while :; do
  dwm
done
