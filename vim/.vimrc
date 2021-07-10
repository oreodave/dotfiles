filetype off

"" Plugins
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
    "" Core
Plugin 'gmarik/Vundle.vim' 
Plugin 'tpope/vim-dispatch'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'christoomey/vim-tmux-navigator'

    "" Plugins
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
call vundle#end()


"" Standard variables
syntax enable
filetype plugin indent on
set nocompatible
set ignorecase
set smartcase
set clipboard=unnamed
set number
set nobackup
set noundofile
set nowritebackup
set nohlsearch
set wildmenu
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set previewheight=5
set foldmethod=syntax
set foldlevel=99
set path+=**
let mapleader = ' '


"" Theming
colorscheme elflord


"" Dir Config
let g:NERDTreeHijackNetrw = 1

"" Keybinds
imap jk <Esc>
nmap <leader>f :e 
