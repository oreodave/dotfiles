# .zprofile -*- mode: sh; lexical-binding: t; -*-

. "$HOME/.local/scripts/user-env"

# Only greet a real terminal; LightDM's Xsession also runs this via
# `$SHELL --login -c`, where the output just lands in ~/.xsession-errors.
if [ -t 1 ]
then
    echo "Welcome to..."
    figlet "Arch Linux"
    fastfetch
fi
