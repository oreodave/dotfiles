#!/bin/sh

setxkbmap gb -option shift:both_capslock -option caps:none;
xmodmap ~/.Xmodmap;

systemctl start --user emacs &
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &

while true; do
    $HOME/.local/scripts/reboot_services &
    ssh-agent dwm || break
done
