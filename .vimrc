set nocompatible
execute pathogen#infect()
let g:ctrlp_dotfiles=0
let g:ctrlp_match_window_reversed=0
let g:easytags_async=1
let g:easytags_dynamic_files=1
let g:molokai_original=1
let g:ycm_path_to_python_interpreter='/usr/bin/python2'
let mapleader=","
map <Leader>m :make<CR>
colorscheme molokai
filetype plugin indent on
command W w
command Q q
command WQ wq
inoremap jj <ESC>
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
nmap <C-x><C-p> :bprev<CR>
nmap <C-x><C-n> :bnext<CR>
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
nnoremap / /\v
nnoremap ? ?\v
vnoremap / /\v
vnoremap ? ?\v
set backspace=indent,eol,start
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8
set history=10000 laststatus=2
set ignorecase infercase smartcase
set incsearch hlsearch showmatch wrapscan
set mouse=a ttymouse=xterm2
set number ruler showcmd showmode
set scrolloff=3 tabstop=2 shiftwidth=2 softtabstop=2 expandtab smarttab smartindent
set ttimeout ttimeoutlen=100
set ttyfast visualbell
set whichwrap=b,s,<,>,[,]
set wildmenu wildmode=longest,list:longest
set undofile nobackup
syntax on
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction
augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END
