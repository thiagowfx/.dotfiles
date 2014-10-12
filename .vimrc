set nocompatible
execute pathogen#infect()
let g:easytags_async=1
let g:easytags_dynamic_files=1
let g:ycm_path_to_python_interpreter='/usr/bin/python2'
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Wq! wq!
cnoreabbrev wQ! wq!
cnoreabbrev WQ! wq!
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
colorscheme murphy
filetype plugin indent on
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
set autoindent
set backspace=indent,eol,start
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8
set gcr=a:blinkon0
set history=1000
set ignorecase smartcase
set incsearch hlsearch showmatch
set laststatus=2
set nobackup
set number
set ruler showcmd showmode
set scrolloff=3
" set smarttab
set ttimeout ttimeoutlen=100
set wildmenu wildmode=longest,list:longest
syntax on
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
endif
if has("autocmd")
  au FileType text setlocal textwidth=72
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
