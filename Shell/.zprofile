# .zprofile -*- mode: sh; lexical-binding: t; -*-

# Standard Variables
export EDITOR='emacsclient -s MAIN -a "emacs" -c'
export EA_EDITOR=$EDITOR
export TERMINAL="st"
export BROWSER="qutebrowser"
export PF_INFO="ascii title os memory uptime editor wm shell"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:$HOME/.local/bin/:$HOME/.emacs.d/bin:$(find ~/.local/scripts/ -type d | tr '\n' ':')$HOME/.cargo/bin"
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
/usr/bin/pfetch
eval `ssh-agent`;
ssh-add ~/.ssh/id_rsa
