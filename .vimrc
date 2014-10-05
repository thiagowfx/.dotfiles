set nocompatible
execute pathogen#infect()

cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
colorscheme murphy
filetype plugin indent on
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
set backspace=indent,eol,start
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
set wildmode=full
syntax on
if has("autocmd")
  augroup vimrcEx
  au!
  autocmd FileType text setlocal textwidth=72
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
  augroup END
else
  set autoindent
endif
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif
