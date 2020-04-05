set nocompatible                " Enable Vim mode (instead of vi emulation)
filetype off                    " Required

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

Plugin 'itchyny/lightline.vim'  " Bottom status line
Plugin 'ap/vim-css-color'       " Color css preview

call vundle#end()
filetype plugin indent on

let g:is_posix = 1              " Our /bin/sh is POSIX, not bash
set autoindent                  " Intelligent indentation matching
set autoread                    " Update the file if it's changed externally
set backspace=indent,eol,start  " Allow backspacing over anything
set belloff=all                 " Turn off bells
set display=truncate            " Show '@@@' when the last screen line overflows
set formatoptions+=j            " Delete comment char when joining lines
set history=100                 " Undo up to this many commands
set hlsearch                    " Highlight search results
set incsearch                   " Highlight search matches as you type them
set ruler                       " Show cursor position
set ttyfast                     " Redraw faster for smoother scrolling
set wildmenu                    " Show menu for tab completion in command mode
set tabstop=2                   " Show existing tab with 2 spaces width
set shiftwidth=2                " When indenting with ->, use 2 spaces width
set softtabstop=2               " Determines how many spaces to use
set expandtab                   " Change tabs for spaces
set laststatus=2                " Bottom status line
set noshowmode                  " Remove default status line
set shortmess=F                 " Remove filename

try                   
  syntax on                     " Enable syntax highlighting
catch | endtry                  " vim-tiny is installed without the syntax files

if v:lang =~ "utf8$" || v:lang =~ "UTF-8$"
  set fileencodings=ucs-bom,utf-8,latin1
endif

" CTRL-L will mute highlighted search results
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

augroup FreeBSD
  autocmd!
    autocmd BufNewFile /usr/ports/*/*/Makefile 0r /usr/ports/Templates/Makefile
    if !empty($PORTSDIR)
      autocmd BufNewFile $PORTSDIR/*/*/Makefile 0r $PORTSDIR/Templates/Makefile
    endif
augroup END
