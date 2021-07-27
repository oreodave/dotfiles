# zshrc -*- mode: sh; lexical-binding: t; -*-

export DOTNET_ROOT=~/.local/src/dotnet
## Variables
TERM=xterm-256color
## Aliases
alias vim="nvim"
alias less="less -R"
alias sudo="doas"
alias eclient="emacsclient -s MAIN"
alias emdir=".config/emacs/"
alias clip="xclip -sel clip"
alias paste="xclip -o"
alias md="mkdir"
alias ls="ls --color"
alias l="ls -la"
alias fzf="fd --hidden | fzf --layout=reverse --height=20"
alias suctl="systemctl --user"
alias sedit="emacsclient -s MAIN -a emacs -c"
alias cedit="emacsclient -s MAIN -a emacs -nw"

vf () {
    vim $(fzf)
}

ef () {
    sedit $(fzf)
}

psearch () {
    pacman -Ss $@ | less
}

### Git aliases
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gg="emacsclient -s MAIN -a emacs -c --eval '(magit)'"

## ZSH
autoload -U colors && colors
autoload -U compinit

setopt autocd
export ZSH_THEME="af-magic"
PS1="%B%F{128}(%n@%m)%B%F{64}[%(4~|...|)%3~]
%F{white}>> %b%f%k"
setopt histignorealldups sharehistory

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

## Imports
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
source /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
#
## Vim binds
bindkey -v
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line
# Menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-up-line-or-history
bindkey -M menuselect 'k' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

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

## NVM config
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/home/dx/.sdkman"
[[ -s "/home/dx/.sdkman/bin/sdkman-init.sh" ]] && source "/home/dx/.sdkman/bin/sdkman-init.sh"
