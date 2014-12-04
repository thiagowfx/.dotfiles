set nocompatible
execute pathogen#infect()
let g:ctrlp_dotfiles=0
let g:ctrlp_match_window_reversed=0
let g:easytags_async=1
let g:easytags_dynamic_files=1
let g:ycm_path_to_python_interpreter='/usr/bin/python2'
let NERDTreeShowHidden=1
map <Leader>a ggVG
map <Leader>m :make<CR>
map <Leader>s :sort<CR>
map <Leader>x :NERDTreeToggle<CR>
colorscheme molokai
filetype plugin indent on
command W w
command Q q
command Wq wq
command WQ wq
map <S-Insert> <MiddleMouse>
map! <S-Insert> <MiddleMouse>
nmap <C-x><C-p> :bprev<CR>
nmap <C-x><C-n> :bnext<CR>
nnoremap / /\v
nnoremap ? ?\v
vnoremap / /\v
vnoremap ? ?\v
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
inoremap jj <ESC>
set autoread autowrite
set backspace=indent,eol,start
set encoding=utf-8 fileencoding=utf-8 fileencodings=utf-8
set nofoldenable foldmethod=indent
set history=10000 laststatus=2 modelines=0
set ignorecase infercase smartcase
set incsearch hlsearch showmatch wrapscan
set mouse=a ttymouse=xterm2
set cursorline number ruler showcmd showmode scrolloff=5
set shiftwidth=2 shiftround tabstop=2 softtabstop=2 expandtab smarttab smartindent
set t_Co=256
set ttimeout ttimeoutlen=100 ttyfast visualbell
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
