""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim-Plug for managing plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set the runtime path to include vim-plug and initialize it
set nocompatible                          " required
filetype off                              " required

call plug#begin('~/.config/nvim/plugged') " required all plugins must appear after this line

Plug 'itchyny/lightline.vim'  " Bottom status line
Plug 'scrooloose/nerdtree'    " Nerdtree

call plug#end()
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Remap Keys
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap ESC
:imap jk <ESC>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-n> :NERDTreeToggle<CR>
let NERDTreeMinimalUI=1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntax Styling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try                   
  syntax on                     " Enable syntax highlighting
catch | endtry                  " vim-tiny is installed without the syntax files

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Powerline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2                " Bottom status line
set noshowmode                  " Remove default status line
set shortmess=F                 " Remove filename

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tabs and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set tabstop=2                   " Show existing tab with 2 spaces width
set shiftwidth=2                " When indenting with ->, use 2 spaces width
set softtabstop=2               " Determines how many spaces to use
set expandtab                   " Change tabs for spaces

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim Configs
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set autoread                    " Update the file if it's changed externally
set backspace=indent,eol,start  " Allow backspacing over anything
set belloff=all                 " Turn off bells
set display=truncate            " Show '@@@' when the last screen line overflows
set formatoptions+=j            " Delete comment char when joining lines
set history=100                 " Undo up to this many commands
set hlsearch                    " Highlight search results
set incsearch                   " Highlight search matches as you type them
set ttyfast                     " Redraw faster for smoother scrolling
set wildmenu                    " Show menu for tab completion in command mode
set number relativenumber
set nu rnu

if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
  set fileencodings=ucs-bom,utf-8
endif
