#!/bin/sh

sh .xprofile;
systemctl start --user emacs &
$(xss-lock --transfer-sleep-lock -- lock) &
feh --randomize --bg-scale $HOME/Pictures/Backgrounds &

while true; do
    $HOME/.local/scripts/reboot_services &
    ssh-agent dwm || break
done
