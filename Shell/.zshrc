# zshrc -*- mode: sh; lexical-binding: t; -*-

## Aliases
alias vi="nvim"
alias vim="nvim"
alias less="less -R"
alias sudo="doas"
alias clip="xclip -sel clip"
alias paste="xclip -o"
alias md="mkdir"
alias ls="ls --color --group-directories-first"
alias l="ls -la --color --group-directories-first"
alias hlight="grep -i -C10 --color=always"
alias fdf="fd --hidden | fzf --layout=reverse --height=20"
alias suctl="systemctl --user"
alias sedit="emacsclient -a emacs -c"
alias cedit="emacsclient -a emacs -nw"
alias dedit="emacsclient -a emcas -nw --eval '(dired \".\" )'"
TERM=xterm-256color

psearch () {
    pacman -Ss $@ | less
}

## Git aliases
alias gs="git status"
alias gd="git diff"
alias gc="git commit"
alias gg="emacsclient -s MAIN -a emacs -c --eval '(magit)'"

## ZSH
autoload -U colors && colors
autoload -U compinit

setopt autocd
export ZSH_THEME="af-magic"
PS1="%B%F{4}[%(4~|...|)%3~]
%F{white}%F{2}%m%b%f> %k"

setopt BANG_HIST
setopt EXTENDED_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_SAVE_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
HISTFILE="$HOME/.zsh_history"
HISTSIZE=1000000000
SAVEHIST=$HISTSIZE
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

## Vim binds
bindkey -v
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line
bindkey "^[[Z" forward-char
# Menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'j' vi-up-line-or-history
bindkey -M menuselect 'k' vi-down-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -a 'k' history-substring-search-up
bindkey -a 'j' history-substring-search-down
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down

## Cursor
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

## SDK man
export SDKMAN_DIR="/home/oreo/.sdkman"
[[ -s "/home/oreo/.sdkman/bin/sdkman-init.sh" ]] && source "/home/oreo/.sdkman/bin/sdkman-init.sh"

if command -v opam 2>&1 >/dev/null
then
eval $(opam env)
fi

[ -f "/home/oreo/.ghcup/env" ] && . "/home/oreo/.ghcup/env" # ghcup-env
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"
