" Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'ack.vim'
Plugin 'ag.vim'
Plugin 'ctrlp.vim'
Plugin 'minibufexpl.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
call vundle#end()
filetype plugin indent on

" Mine
colorscheme murphy
set ai
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
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
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
