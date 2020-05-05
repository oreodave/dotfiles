# zshrc -*- mode: sh; lexical-binding: t; -*-

## Important variables and stuff
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.local/bin:~/Bin/binaries:~/.emacs.d/bin:~/.cargo/bin:~/Scripts
export guile=guile2.2
export PF_INFO="ascii title os memory uptime editor wm shell"
export EDITOR="emacs"
export SHELL="zsh"
alias yapf='python2 -m yapf'
alias clip="xclip -sel clip"
alias paste="xclip -o"
alias ls="ls --color=auto"
alias l="ls -la"
alias md="mkdir"
alias fzf="fzf --layout=reverse --height=20"
alias suctl="systemctl --user"
export force_color_prompt=yes
export ZSH_THEME="af-magic"
export XDG_RUNTIME_DIR=/run/user/`id -u`
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1

## ZSH
PS1="%F{8}[δx@%m%k] %B%F{14}[%(4~|...|)%3~]%F{white}
λ %b%f%k"
setopt histignorealldups sharehistory

autoload -U colors && colors
autoload -U compinit
HISTSIZE=10000
SAVEHIST=10000
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2 eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,'

## Vim binds
bindkey -v
# Menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-up-line-or-history
bindkey -M menuselect 'k' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char

# Cursor
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}

zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

## Programming
editor() {
    nohup emacs $@ > /dev/null &
}

gentemplate() {
    for var in ${@:2}; do
        case $1 in
            'c') git clone https://github.com/Oreodave/CTemplate $var;;
            'cpp') git clone https://github.com/Oreodave/CPPTemplate $var;;
            'python') git clone https://github.com/Oreodave/PythonTemplate $var;;
            'node') git clone https://github.com/Oreodave/NodeTemplate $var;;
            'ard') git clone https://github.com/Oreodave/ArduinoTemplate $var;;
            'java') git clone https://github.com/Oreodave/JavaTemplate $var;;
            *) return;;
        esac
        rm -rf $var/.git;
    done
}

gentemplateoff () {
    for var in ${@:2}; do
        case $1 in
            'c') cp -r ~/Code/Templates/CTemplate $var;;
            'cpp') cp -r ~/Code/Templates/CPPTemplate $var;;
            'python') cp -r ~/Code/Templates/PythonTemplate $var;;
            'node') cp -r ~/Code/Templates/NodeTemplate $var;;
            'ard') cp -r ~/Code/Templates/ArduinoTemplate $var;;
            'java') cp -r ~/Code/Templates/JavaTemplate $var;;
            *) return;;
        esac
        rm -rf $var/.git;
    done
}

## NVM config
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
