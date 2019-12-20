# zshenv -*- mode: sh; lexical-binding: t; -*-
# Important variables
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.local/bin:~/.bin:~/.emacs.d/bin
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1
export EDITOR="vim"
export SHELL="zsh"
alias yapf='python2 -m yapf'
alias clip="xclip -sel clip"
alias e="emacsclient -c "
alias paste="xclip -o"
export force_color_prompt=yes

# Programming
SPC() {
    emacs $1 & disown > /dev/null;
}

gentemplate() {
    case $1 in
        'c') git clone https://github.com/Oreodave/CTemplate $2;;
        'cpp') git clone https://github.com/Oreodave/CPPTemplate $2;;
        'python') git clone https://github.com/Oreodave/PythonTemplate $2;;
        'node') git clone https://github.com/Oreodave/NodeTemplate $2;;
        'ard') git clone https://github.com/Oreodave/ArduinoTemplate $2;;
        *) return;;
    esac
    rm -rf $2/.git;
}

gentemplateoff () {
    case $1 in
        'c') cp -r ~/Projects/Templates/CTemplate $2;;
        'cpp') cp -r ~/Projects/Templates/CPPTemplate $2;;
        'python') cp -r ~/Projects/Templates/PythonTemplate $2;;
        'node') cp -r ~/Projects/Templates/NodeTemplate $2;;
        'ard') cp -r ~/Projects/Templates/ArduinoTemplate $2;;
        *) return;;
    esac
    rm -rf $2/.git;
}

# Web Querying
search () {
    search_query=${1// /_}
    web "duckduckgo.com/$search_query"
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
