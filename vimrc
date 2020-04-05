filetype off

"" Plugins
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
    "" Core
Plugin 'gmarik/Vundle.vim' 
Plugin 'tpope/vim-dispatch'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'

    "" UI
Plugin 'scrooloose/nerdtree'

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
let mapleader = ' '


"" Theming
colorscheme elflord


"" Dir Config
let g:NERDTreeHijackNetrw = 1


"" Keybinds
imap jk <Esc>
    "" General keybind
nnoremap <leader>fp :e ~/.vimrc<CR>
nnoremap <leader>fei :e ~/.vim/ftplugin<CR>
nnoremap <leader>fr :so ~/.vimrc<CR>:PluginInstall<CR>
nnoremap <leader>qq :q!<CR>
nnoremap <leader>gs :G<CR>
nnoremap <leader><leader> :

    "" File Management
nnoremap <leader>ff :e 
nnoremap <leader>fs :w<CR>
nnoremap <leader>fq :wq<CR>
nnoremap <leader>fn :enew<CR>
nnoremap <F8> :set hlsearch! hlsearch?<CR>

    "" Buffer Management
nnoremap <leader>bb :b 
nnoremap <leader>bn :bn <CR>
nnoremap <leader>bp :bp <CR>
nnoremap <leader>bd :bd <CR>

    "" Search
nnoremap <leader>ss /

    "" Window Splits
nnoremap <leader>wv <C-W>v
nnoremap <leader>ws <C-W>s
nnoremap <leader>wd <C-W>q

    "" Window Resizes
nnoremap <leader>wj :resize -5<CR>
nnoremap <leader>wk :resize +5<CR>
nnoremap <leader>wl <C-W>5>
nnoremap <leader>wh <C-W>5<

    "" Window Movement
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>

    "" Projects
nnoremap <leader>ot :NERDTreeToggle<CR>
nnoremap <leader>pg :!ctags-exuberant -R --exclude=Makefile .

    "" Tags
nnoremap <leader>tt :Tags<CR>
