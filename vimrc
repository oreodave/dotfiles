filetype off

"" Plugins
set rtp+=~/.vim/bundle/Vundle.vim
set rtp+=~/.fzf

call vundle#begin()
    "" Core
Plugin 'gmarik/Vundle.vim' 
Plugin 'junegunn/fzf.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-dispatch'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'w0rp/ale'

    "" UI
Plugin 'scrooloose/nerdtree'
Plugin 'crusoexia/vim-monokai'
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
let mapleader = ' '


"" Theming
let g:airline_theme = 'vorange'
colorscheme monokai


"" Dir Config
let g:NERDTreeHijackNetrw = 1


"" Language Config
let g:OmniSharp_server_stdio = 1
let g:OmniSharp_selector_ui = 'fzf'


"" Tool Config
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:ale_linters = {
            \ 'cs': ['OmniSharp']
            \}


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
nnoremap <leader>fr :e!<CR>
nnoremap <leader>ff :Files . <CR>
nnoremap <leader>fs :w<CR>
nnoremap <leader>fq :wq<CR>
nnoremap <leader>fn :enew<CR>
nnoremap <F8> :set hlsearch! hlsearch?<CR>

    "" Buffer Management
nnoremap <leader>bb :Buffers<CR>
nnoremap <leader>bn :bn <CR>
nnoremap <leader>bp :bp <CR>
nnoremap <leader>bd :bd <CR>

    "" Window Management
nnoremap <leader>ww :Windows<CR>
        "" Splits
nnoremap <leader>wv <C-W>v
nnoremap <leader>ws <C-W>s
nnoremap <leader>wd <C-W>q
        "" Resizes
nnoremap <leader>wj :resize -5<CR>
nnoremap <leader>wk :resize +5<CR>
nnoremap <leader>wl <C-W>5>
nnoremap <leader>wh <C-W>5<
        "" Movement
nnoremap <leader>j <C-W><C-J>
nnoremap <leader>k <C-W><C-K>
nnoremap <leader>l <C-W><C-L>
nnoremap <leader>h <C-W><C-H>
    "" Searches
nnoremap <leader>// :BLines <CR>
nnoremap <leader>/a :Ag <CR>
nnoremap <leader>/r :Rg <CR>
nnoremap <leader>/t :BTags <CR>
nnoremap <leader>fF :Files 

    "" Projects
nnoremap <leader>pf :Files<CR>
nnoremap <leader>ot :NERDTreeToggle<CR>
nnoremap <leader>pg :!ctags-exuberant -R --exclude=Makefile .

    "" Tags
nnoremap <leader>tt :Tags<CR>
