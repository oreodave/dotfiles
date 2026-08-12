#!/usr/bin/env sh

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export EDITOR='emacsclient -a "emacs" -c'
export EA_EDITOR="$EDITOR"
export BROWSER="waterfox"
export TERMINAL="st"
export WEBKIT_FORCE_SANDBOX=0
export PF_INFO="ascii title os memory uptime editor wm shell"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin/vendor_perl/:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:$HOME/.local/bin/:$(find ~/.local/scripts/ -type d | tr '\n' ':')$HOME/.cargo/bin:$HOME/.sdkman/bin"
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMETRY_OPTOUT=1
export DOTNET_ROOT="$HOME/.local/src/dotnet"
export force_color_prompt=yes
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"
[ -r /etc/profile.d/debuginfod.sh ] && . /etc/profile.d/debuginfod.sh
