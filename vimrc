filetype off

" Plugins
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf

call vundle#begin()
    " Core
Plugin 'gmarik/Vundle.vim'
Plugin 'wakatime/vim-wakatime'
Plugin 'junegunn/fzf.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'Valloric/YouCompleteMe'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'w0rp/ale'

    " UI
Plugin 'scrooloose/nerdtree'
Plugin 'vim-airline/vim-airline'
Plugin 'powerline/powerline'
Plugin 'mhinz/vim-startify'
Plugin 'morhetz/gruvbox'

    " Languages
Plugin 'plasticboy/vim-markdown'
Plugin 'jceb/vim-orgmode'
Plugin 'OmniSharp/omnisharp-vim'

    " Plugins 
Plugin 'godlygeek/tabular'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-speeddating'

    " Other
Plugin 'rhysd/vim-clang-format'
Plugin 'python-rope/ropevim'
call vundle#end()


" Standard variables
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
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set foldmethod=syntax
set foldlevel=99
let mapleader = ' '


" Theming
let g:airline_theme = 'vorange'
colorscheme gruvbox


" Dir Config
let g:NERDTreeHijackNetrw = 1


" Language Config
let g:OmniSharp_server_stdio = 1
let g:OmniSharp_selector_ui = 'fzf'


" Tool Config
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'


" Keybinds
    " General keybind
map <leader>fed :e ~/.vimrc<CR>
map <leader>fep :e ~/.vim/ftplugin<CR>
map <leader>fer :so ~/.vimrc<CR>:PluginInstall<CR>

    " File Management
map <leader>fr :e!<CR>
map <leader>fs :w<CR>
map <leader>fq :wq<CR>
map <leader>fn :enew<CR>
map <F8> :set hlsearch! hlsearch?<CR>

    " Buffer Management
map <leader>bb :Buffers<CR>
map <leader>bn :bn <CR>
map <leader>bp :bp <CR>
map <leader>bd :bd <CR>

    " Window Management
map <leader>ww :Windows<CR>
        " Splits
map <leader>wv <C-W>v
map <leader>wh <C-W>s
map <leader>wd <C-W>q
        " Resizes
map <leader>w<Up> :resize -5<CR>
map <leader>w<Down> :resize +5<CR>
map <leader>w<Right> <C-W>5>
map <leader>w<Left> <C-W>5<
        " Movement
map <leader>wj <C-W><C-J>
map <leader>wk <C-W><C-K>
map <leader>wl <C-W><C-L>
map <leader>wh <C-W><C-H>

    " Searches
map <leader>ss :BLines <CR>
map <leader>sf :Lines <CR>
map <leader>ff :Files<CR>

    " Nerd Tree Config
map <leader>tt :NERDTreeToggle<CR>
map <leader>tf :NERDTreeFocus<CR>

