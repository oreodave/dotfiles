#!/bin/sh

killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --output HDMI1    --auto --mode 1920x1080 --left-of eDP1;
xrandr --output eDP1     --auto --mode 1920x1080;
xrandr --output HDMI-1   --auto --mode 1920x1080 --left-of HDMI-1-0;
xrandr --output HDMI-1-0 --auto --mode 1920x1080 --right-of HDMI-1;

sxhkd &
dunst &
$(xss-lock --transfer-sleep-lock -- $HOME/.local/scripts/lock) &
dwmblocks &
$HOME/.local/scripts/compositor &
$HOME/.local/scripts/background &

while :
do
  dwm
done
