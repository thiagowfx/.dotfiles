set nocompatible
execute pathogen#infect()
let g:ycm_path_to_python_interpreter = '/usr/bin/python2'
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoremap <C-n> <Down>
colorscheme murphy
filetype plugin indent on
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
set autoindent
set backspace=indent,eol,start
set encoding=utf-8
set fileencoding=utf-8
set fileencodings=utf-8
set gcr=a:blinkon0
set history=200
set hlsearch
set ignorecase
set incsearch
set nobackup
set number
set ruler
set paste
set scrolloff=3
set showcmd
set showmatch
set showmode
set smartcase
set wildmenu
set wildmode=longest,list:longest
syntax on
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
endif
if has("autocmd")
  au FileType text setlocal textwidth=72
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
