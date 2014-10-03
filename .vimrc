set nocompatible
set backspace=indent,eol,start
set history=50
set nobackup
set ruler
set showcmd
set showmode
set incsearch
set nu
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif
if has ("gui_running")
  map <S-Insert> <MiddleMouse>
  map! <S-Insert> <MiddleMouse>
endif
if has("autocmd")
  filetype plugin indent on
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
