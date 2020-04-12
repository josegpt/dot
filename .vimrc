""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vundle for managing plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set nocompatible                " Enable Vim mode (instead of vi emulation)
filetype off                    " Required

" set the runtime path to include Vundle and initialize it
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin() " required all plugins must appear after this line

Plugin 'VundleVim/Vundle.vim'   " Initializing Vundle
Plugin 'itchyny/lightline.vim'  " Bottom status line
Plugin 'scrooloose/nerdtree'    " Nerdtree

call vundle#end()
filetype plugin indent on       " Required

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Remap Keys
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Remap ESC ii
:imap jk <ESC>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => NERDTree
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
map <C-n> :NERDTreeToggle<CR>
let NERDTreeMinimalUI=1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Syntax Styling
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=16                    " Uses Xresources colors
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
set relativenumber

if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
  set fileencodings=ucs-bom,utf-8
endif

augroup FreeBSD
  autocmd!
    autocmd BufNewFile /usr/ports/*/*/Makefile 0r /usr/ports/Templates/Makefile
    if !empty($PORTSDIR)
      autocmd BufNewFile $PORTSDIR/*/*/Makefile 0r $PORTSDIR/Templates/Makefile
    endif
augroup END
