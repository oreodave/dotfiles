# .zprofile -*- mode: sh; lexical-binding: t; -*-

# XDG variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_RUNTIME_DIR=/run/user/`id -u`

# Standard Variables
export EDITOR='emacsclient -a "emacs" -c'
export EA_EDITOR=$EDITOR
export TERMINAL="alacritty"
export BROWSER="chromium"
export WEBKIT_FORCE_SANDBOX=0
export PF_INFO="ascii title os memory uptime editor wm shell"
export PATH="/usr/local/sbin:/usr/local/bin:/usr/bin/vendor_perl/:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:$HOME/.local/bin/:$(find ~/.local/scripts/ -type d | tr '\n' ':')$HOME/.cargo/bin:$HOME/.sdkman/bin"
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1
export force_color_prompt=yes
export DOTNET_ROOT=~/.local/src/dotnet
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR}/ssh-agent.socket"

# Setup debuginfod
sh /etc/profile.d/debuginfod.sh

# Import path to systemd
systemctl --user import-environment PATH SSH_AUTH_SOCK

# Run some programs
echo "Welcome to..."
figlet "Arch Linux"
/usr/bin/pfetch
fortune | cowsay -f "tux"
