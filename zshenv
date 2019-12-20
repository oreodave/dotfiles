# zshenv -*- mode: sh; lexical-binding: t; -*-
# Important variables
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.local/bin:~/.bin:~/.emacs.d/bin
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1
export EDITOR="vim"
export SHELL="zsh"
alias yapf='python2 -m yapf'
alias clip="xclip -sel clip"
alias paste="xclip -o"
export force_color_prompt=yes

# Programming
spc() {
    emacsclient -c --socket-name=MAIN $1 & disown
}

spu() { # Use
    emacsclient --socket-name=MAIN $1 & disown
}

gentemplate() {
    for var in ${@:2}; do
        case $1 in
            'c') git clone https://github.com/Oreodave/CTemplate $var;;
            'cpp') git clone https://github.com/Oreodave/CPPTemplate $var;;
            'python') git clone https://github.com/Oreodave/PythonTemplate $var;;
            'node') git clone https://github.com/Oreodave/NodeTemplate $var;;
            'ard') git clone https://github.com/Oreodave/ArduinoTemplate $var;;
            *) return;;
        esac
        rm -rf $var/.git;
    done
}

gentemplateoff () {
    for var in ${@:2}; do
    case $1 in
        'c') cp -r ~/Projects/Templates/CTemplate $var;;
        'cpp') cp -r ~/Projects/Templates/CPPTemplate $var;;
        'python') cp -r ~/Projects/Templates/PythonTemplate $var;;
        'node') cp -r ~/Projects/Templates/NodeTemplate $var;;
        'ard') cp -r ~/Projects/Templates/ArduinoTemplate $var;;
        *) return;;
    esac
    rm -rf $var/.git;
    done
}

# Web Querying
search () {
    web "duckduckgo.com/?q=$1"
}

web () {
    firefox $1 > /dev/null & disown
}

😂() {
    web "https://youtu.be/qMc6xlZaxYA"
}


# OPAM configuration
. /home/oreodave/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

#NVM config
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
