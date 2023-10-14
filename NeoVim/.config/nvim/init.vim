set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
filetype off

"" Plugins
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
    "" Core
Plugin 'gmarik/Vundle.vim' 
Plugin 'tpope/vim-dispatch'
Plugin 'christoomey/vim-tmux-navigator'

    "" UI
Plugin 'scrooloose/nerdtree'

    "" Plugins
Plugin 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plugin 'junegunn/fzf.vim'
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'
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
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
set previewheight=5
set foldmethod=syntax
set foldlevel=99
set path+=**
let mapleader = ' '


"" Theming
set background=dark
colorscheme koehler

"" Dir Config
let g:NERDTreeHijackNetrw = 1


"" Keybinds
imap jk <Esc>
    "" General keybind
nnoremap <leader>fp :e ~/.config/nvim/init.vim<CR>
nnoremap <leader>fei :e ~/.vim/ftplugin<CR>
nnoremap <leader>fr :so ~/.vimrc<CR>:PluginInstall<CR>:PluginClean<CR>
nnoremap <leader>qq :q!<CR>
nnoremap <leader>gs :G<CR>
nnoremap <leader><leader> :

    "" File Management
nnoremap <leader>ff :Files <CR> 
nnoremap <leader>fs :w<CR>
nnoremap <leader>fq :wq<CR>
nnoremap <leader>fn :enew<CR>
nnoremap <F8> :set hlsearch! hlsearch?<CR>

    "" Buffer Management
nnoremap <leader>bn :bn <CR>
nnoremap <leader>bp :bp <CR>
nnoremap <leader>bb :Buffers <CR>
nnoremap <leader>bd :bd <CR>

    "" Search
nnoremap <leader>ss :Lines<CR>

    "" Window Splits
nnoremap <leader>wv <C-W>v
nnoremap <leader>ws <C-W>s
nnoremap <leader>wd <C-W>q

    "" Window Movement
nnoremap <leader>wj <C-W><C-J>
nnoremap <leader>wk <C-W><C-K>
nnoremap <leader>wl <C-W><C-L>
nnoremap <leader>wh <C-W><C-H>

    "" Projects
nnoremap <leader>ot :NERDTreeToggle<CR>
nnoremap <leader>pg :!ctags-exuberant -R --exclude=Makefile .

    "" Tags
nnoremap <leader>tt :Tags<CR>
