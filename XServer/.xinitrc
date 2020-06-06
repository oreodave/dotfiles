#!/bin/sh

sh .xprofile;
systemctl start --user emacs &
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &

while true; do
    killall picom;
    killall mpd;
    killall sxhkd;
    killall dwmblocks;
    picom &
    mpd &
    sxhkd &
    ssh-agent dwm || break
done
