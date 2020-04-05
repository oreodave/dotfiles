# zshenv -*- mode: sh; lexical-binding: t; -*-
# Important variables
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.local/bin:~/Bin/binaries:~/.emacs.d/bin:~/.cargo/bin:~/Scripts
export guile=guile2.2
export PF_INFO="ascii title os memory uptime editor shell"
export EDITOR="emacs"
export SHELL="zsh"
alias yapf='python2 -m yapf'
alias clip="xclip -sel clip"
alias paste="xclip -o"
export force_color_prompt=yes
export ZSH_THEME="af-magic"
export XDG_RUNTIME_DIR=/run/user/`id -u`
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1

# Programming
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

#NVM config
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
