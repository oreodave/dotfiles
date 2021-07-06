#!/bin/sh

killall picom;
killall sxhkd;

sh .xprofile;
xset s 1800;
xrandr --output HDMI1 --primary;
xrandr --output eDP1 --right-of HDMI1;

$(xss-lock --transfer-sleep-lock -- lock) &
$HOME/.local/scripts/background &

systemctl --user start emacs &
$HOME/.local/scripts/status/music_update_bar &
dunst &
picom &
sxhkd &
dwmblocks &

# $HOME/.config/bspwm/bspwmrc
while :; do
  ssh-agent dwm
done
