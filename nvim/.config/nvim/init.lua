-- Neovim config file

-- Set leader key. Default is '\'.
vim.g.mapleader = ","

-- The colorscheme of lightline is independent of the editor colorscheme.
vim.g.lightline = { colorscheme = 'onedark' }

-- :w save misspellings
vim.cmd("command! GWQ Gwq")
vim.cmd("command! W w")
vim.cmd("command! WQ wq")
vim.cmd("command! Wq wq")

-- https://vi.stackexchange.com/questions/439/how-to-join-lines-without-producing-a-space
vim.keymap.set('n', 'J', 'gJ', { noremap = true })

-- Fix whitespace with <leader>w
vim.keymap.set('n', '<leader>w', ':FixWhitespace<CR>', { noremap = true })

-- Reflow current paragraph
vim.keymap.set('n', 'Q', 'gwip', { noremap = true, silent = true })
vim.keymap.set('n', 'W', '!ipfmt<Enter>', { noremap = true, silent = true })
vim.keymap.set('n', 'E', '!ipfold -s<Enter>', { noremap = true, silent = true })

-- a la emacs just-one-space
local function just_one_space()
  local line = vim.fn.getline('.')
  local col = vim.fn.col('.')

  -- Find start of whitespace sequence
  local start = col
  while start > 1 and string.find(string.sub(line, start - 1, start - 1), '%s') do
    start = start - 1
  end

  -- Find end of whitespace sequence
  local finish = col
  while finish <= #line and string.find(string.sub(line, finish, finish), '%s') do
    finish = finish + 1
  end

  -- Only proceed if we're in a whitespace sequence with more than 1 space
  if finish - start > 1 then
    -- Replace the whitespace sequence with a single space
    local newline = string.sub(line, 1, start - 2) .. ' ' .. string.sub(line, finish)
    vim.fn.setline('.', newline)
    -- Position cursor right after the single space
    vim.fn.cursor(vim.fn.line('.'), start + 1)
  end
end
vim.keymap.set('n', '<leader><Space>', just_one_space, { noremap = true })

-- Always use vertical diff splits.
vim.opt.diffopt:append("vertical")

-- double tap esc to unset highlighting
vim.keymap.set('n', '<Esc><Esc>', ':noh<CR>', { noremap = true })

-- https://stackoverflow.com/a/2288438/1745064
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Display whitespace characters, including tabs.
vim.opt.list = true
vim.opt.listchars = "tab:|\\ ,trail:·,nbsp:·"

-- Disable fold.
vim.opt.foldenable = false

-- Disable --INSERT-- which is not necessary, because:
--   Cursor shape changes thanks to wincent/terminus.
--   itchyny/lightline.vim already displays it.
vim.opt.showmode = false

-- Disable backups and swapfile, use persistent undo instead.
vim.opt.swapfile = false
vim.opt.writebackup = false

-- Show line numbers.
vim.opt.number = true

-- Simplify some display messages.
vim.opt.shortmess:append("atI")

-- Display indicators for lines that have been wrapped.
vim.opt.showbreak = '> '

-- Smart autoindent when starting a new line.
vim.opt.smartindent = true

-- Spell-checking
-- vim.opt.spell = true
-- vim.opt.spelllang = "de_de,en_ca,en_us,pt_br"

-- Better splits.
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.switchbuf = "useopen"

-- Enable persistent undo.
vim.opt.undofile = true
local undo_dir = vim.fn.expand('~/.cache/nvim/undo')
vim.opt.undodir = undo_dir
vim.fn.mkdir(undo_dir, 'p', tonumber('0700', 8))

-- Shorter update time than the 4000ms default, for async operations.
vim.opt.updatetime = 2000

-- Faster mode switching
vim.opt.timeoutlen = 500
vim.opt.ttimeoutlen = 10

-- Set completion mode.
vim.opt.wildmode = "list:longest"

-- Setup lazy.nvim
local lazypath = vim.fn.stdpath("config") .. "/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugin specs
local plugins = {
  -- Linting and fixing
  'dense-analysis/ale',

  -- Git integration
  'mhinz/vim-signify',
  'tpope/vim-fugitive',

  -- Editing
  'tpope/vim-commentary',
  'tpope/vim-surround',
  'tpope/vim-repeat',
  { 'junegunn/vim-easy-align', cmd = { 'EasyAlign' } },

  -- Navigation and search
  'junegunn/fzf',
  'junegunn/vim-slash',
  'justinmk/vim-sneak',

  -- File and buffer management
  'tpope/vim-vinegar',
  'aymericbeaumet/vim-symlink',
  'pbrisbin/vim-mkdir',
  'bogado/file-line',
  'tpope/vim-eunuch',

  -- Code quality and formatting
  'tpope/vim-endwise',
  'bronson/vim-trailing-whitespace',

  -- UI enhancements
  'itchyny/lightline.vim',
  'machakann/vim-highlightedyank',
  'wincent/terminus',

  -- Better defaults
  'tpope/vim-unimpaired',

  -- Clipboard integration
  'ojroques/vim-oscyank',

  -- Color schemes
  { 'joshdick/onedark.vim', name = 'onedark' },

  -- Diff tools
  'whiteinge/diffconflicts',
}

local opts = {
  -- lazy.nvim options here
}

require("lazy").setup(plugins, opts)

-- Configure ale
vim.g.ale_lint_on_enter = 0
vim.g.ale_virtualtext_cursor = 0

-- Configure vim-fugitive custom commands
vim.cmd("command! Gadd Gwrite")
vim.cmd("command! Gdiff Gdiffsplit")
vim.cmd("command! Gwqall Gwq")

-- Configure vim-eunuch custom command
vim.cmd("command! DoasWrite execute 'silent! write !doas tee % >/dev/null' <bar> edit!")

-- Clipboard integration - OSC52
vim.keymap.set('v', '<C-c>', ':OSCYankVisual<CR>gv', { noremap = true })

-- Set color theme / scheme
pcall(function()
  vim.cmd("colorscheme onedark")
  vim.cmd("highlight ColorColumn ctermbg=magenta")
end)
