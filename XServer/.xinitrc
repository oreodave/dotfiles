#!/bin/sh

setxkbmap gb -option shift:both_capslock -option caps:none;
xmodmap ~/.Xmodmap;
systemctl --user restart emacs;

xss-lock --transfer-sleep-lock --  i3lock -n -f -e -i $HOME/Pictures/Lockscreens/milkyway.png -c 555555 &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &
picom & 

while true; do
    $HOME/.local/scripts/reboot_services &
    ssh-agent dwm || break
done
