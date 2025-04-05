#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1;
xrandr --output eDP1  --mode 1920x1080;

$(xss-lock --transfer-sleep-lock -- $HOME/.local/scripts/lock) &
$HOME/.local/scripts/background &

dunst &
picom --backend xrender --no-fading-openclose &
sxhkd &
dwmblocks &

while :
do
  dwm
done
