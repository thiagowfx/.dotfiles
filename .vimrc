set nocompatible
execute pathogen#infect()
let g:easytags_async=1
let g:easytags_dynamic_files=1
let g:ycm_path_to_python_interpreter='/usr/bin/python2'
let mapleader=","
map <Leader>m :make<CR>
inoremap jj <ESC>
colorscheme murphy
filetype plugin indent on
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
cnoreabbrev W! w!
cnoreabbrev Q! q!
cnoreabbrev Wq! wq!
cnoreabbrev wQ! wq!
cnoreabbrev WQ! wq!
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v
set autoindent
set backspace=indent,eol,start
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8
set history=1000
set ignorecase infercase smartcase
set incsearch hlsearch showmatch
set laststatus=2
set number relativenumber ruler showcmd showmode
set scrolloff=3
set tabstop=4 shiftwidth=4 softtabstop=4 expandtab
set ttimeout ttimeoutlen=100
set ttyfast
set visualbell
set wildmenu wildmode=longest,list:longest
set undofile nobackup
syntax on
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
endif
if has("autocmd")
  au FileType text setlocal textwidth=79
  au FocusLost * :wa
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif
