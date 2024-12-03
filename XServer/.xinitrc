#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;

xrandr --output eDP1  --mode 2560x1440 --pos 0x0      --rotate normal;
xrandr --output HDMI1 --mode 1920x1080 --pos 2560x0   --scale 1.33x1.33 --rotate normal;

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
