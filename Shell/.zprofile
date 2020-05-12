# .zprofile -*- mode: sh; lexical-binding: t; -*-

# Standard Variables
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export PF_INFO="ascii title os memory uptime editor wm shell"
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.local/bin:~/.emacs.d/bin:~/.cargo/bin:~/.local/scripts
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1
export force_color_prompt=yes
export guile=guile2.2

# XDG variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR=/run/user/`id -u`

# Run some programs
setxkbmap gb
figlet dx
/usr/bin/pfetch
if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi
