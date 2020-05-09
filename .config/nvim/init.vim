""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim-Plug for managing plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Boostrap Installation
let vimplug_exists=expand('~/.config/nvim/autoload/plug.vim')

let g:vim_bootstrap_langs = "go,html,javascript,python,typescript"
let g:vim_bootstrap_editor = "nvim"				" nvim or vim

if !filereadable(vimplug_exists)
  if !executable("curl")
    echoerr "You have to install curl or first install vim-plug yourself!"
    execute "q!"
  endif
  echo "Installing Vim-Plug..."
  echo ""
  silent exec "!\curl -fLo " . vimplug_exists . " --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
  let g:not_finish_vimplug = "yes"

  autocmd VimEnter * PlugInstall
endif

" set the runtime path to include vim-plug and initialize it
set nocompatible                          " required
filetype off                              " required

call plug#begin('~/.config/nvim/plugged') " required all plugins must appear after this line

" => automatically install missing plugins on startup
if !empty(filter(copy(g:plugs), '!isdirectory(v:val.dir)'))
  autocmd VimEnter * PlugInstall | q
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => My Pluggins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
Plug 'arcticicestudio/nord-vim'
Plug 'itchyny/lightline.vim'  " Bottom status line
Plug 'jremmen/vim-ripgrep'
Plug 'scrooloose/nerdtree'    " Nerdtree
Plug 'tpope/vim-fugitive'
Plug 'w0rp/ale'

" go
"" Go Lang Bundle
Plug 'fatih/vim-go', {'do': ':GoInstallBinaries'}

" html
"" HTML Bundle
Plug 'hail2u/vim-css3-syntax'
Plug 'gorodinskiy/vim-coloresque'
Plug 'tpope/vim-haml'
Plug 'mattn/emmet-vim'

" javascript
"" Javascript Bundle
Plug 'jelera/vim-javascript-syntax'

" typescript
Plug 'leafgarland/typescript-vim'
Plug 'HerringtonDarkholme/yats.vim'


" vuejs
Plug 'posva/vim-vue'
Plug 'leafOfTree/vim-vue-plugin'

call plug#end()


filetype plugin indent on        " Required
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
set guioptions=egmrti
set gfn=Monospace\ 10
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
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8
set backspace=indent,eol,start
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

"" Copy/Paste/Cut
if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
endif
