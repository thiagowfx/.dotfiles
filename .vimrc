" Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'tpope/vim-fugitive'
call vundle#end()
filetype plugin indent on

" Mine
colorscheme murphy
set backspace=indent,eol,start
set history=200
set incsearch
set nobackup
set nu
set ruler
set paste
set showcmd
set showmode
set wildmenu
set wildmode=full
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
if has ("gui_running")
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
endif
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

imap <Tab> <C-P>
