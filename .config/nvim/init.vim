""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim-Plug for managing plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Boostrap Installation
if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source ~/.config/nvim/init.vim
endif

" set the runtime path to include vim-plug and initialize it
set nocompatible                          " required
filetype off                              " required

call plug#begin('~/.config/nvim/plugged') " required all plugins must appear after this line

" => automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  autocmd VimEnter * PlugInstall | q
endif

" => my plugins
Plug 'arcticicestudio/nord-vim'
Plug 'itchyny/lightline.vim'  " Bottom status line
Plug 'jremmen/vim-ripgrep'
Plug 'scrooloose/nerdtree'    " Nerdtree
Plug 'tpope/vim-fugitive'

call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Remap Keys
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:imap jk <ESC>                   " Remap ESC
let mapleader = " "              " Leader Key
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Commands
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
nnoremap <leader>ps :Rg<SPACE>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-n> :NERDTreeToggle<CR>
let NERDTreeMinimalUI=1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Theming
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256
colorscheme nord
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntax Styling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try                   
  syntax on                     " Enable syntax highlighting
catch | endtry                  " vim-tiny is installed without the syntax files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Lightline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2                " Bottom status line
set noshowmode                  " Remove default status line
set shortmess=F                 " Remove filename
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ }
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tabs and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tabstop=2                   " Show existing tab with 2 spaces width
set shiftwidth=2                " When indenting with ->, use 2 spaces width
set softtabstop=2               " Determines how many spaces to use
set expandtab                   " Change tabs for spaces
set smartindent                 " Set smartindent
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim Configs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set hidden
set autoread                    " Update the file if it's changed externally
set noerrorbells                " Turn off bells
set history=100                 " Undo up to this many commands
set hlsearch                    " Highlight search results
set incsearch                   " Highlight search matches as you type them
set ttyfast                     " Redraw faster for smoother scrolling
set wildmenu                    " Show menu for tab completion in command mode
set number relativenumber
set nu rnu
set nowrap                      " no truncate
set smartcase
set noswapfile
set nobackup
set undodir=~/.config/nvim/undodir
set undofile
set incsearch
set colorcolumn=80
