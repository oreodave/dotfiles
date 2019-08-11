# Important variables
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games
export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=true
export DOTNET_CLI_TELEMTRY_OPTOUT=1
alias yapf='python3 -m yapf'
alias clip="xclip -sel clip"
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

# OPAM configuration
. /home/oreodave/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
