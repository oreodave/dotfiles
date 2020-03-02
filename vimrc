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
colorscheme monokai


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
nnoremap <leader>ff :Files .<CR>
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
nnoremap <leader>ss :BLines <CR>
nnoremap <leader>sr :Rg <CR>
nnoremap <leader>st :BTags <CR>
nnoremap <leader>fF :Files 

    "" Projects
nnoremap <leader>pf :Files<CR>
nnoremap <leader>ot :NERDTreeToggle<CR>
nnoremap <leader>pg :!ctags-exuberant -R --exclude=Makefile .

    "" Tags
nnoremap <leader>tt :Tags<CR>
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
