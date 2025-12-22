-- Neovim config file

-- Set leader key. Default is '\'.
vim.g.mapleader = ","

-- :w save misspellings
vim.cmd("command! GWQ Gwq")
vim.cmd("command! W w")
vim.cmd("command! WQ wq")
vim.cmd("command! Wq wq")

-- https://vi.stackexchange.com/questions/439/how-to-join-lines-without-producing-a-space
vim.keymap.set('n', 'J', 'gJ', { noremap = true, silent = true })

-- Fix whitespace with <leader>w
vim.keymap.set('n', '<leader>w', ':FixWhitespace<CR>', { noremap = true, silent = true, desc = 'Fix whitespace' })

-- Reflow current paragraph
vim.keymap.set('n', 'Q', 'gwip', { noremap = true, silent = true })
vim.keymap.set('n', 'W', '!ipfmt<Enter>', { noremap = true, silent = true })
vim.keymap.set('n', 'E', '!ipfold -s<Enter>', { noremap = true, silent = true })

-- a la emacs just-one-space
local function just_one_space()
  local line = vim.api.nvim_get_current_line()
  local col = vim.fn.col('.')

  local s, e = col, col
  while s > 1 and line:sub(s - 1, s - 1):match('%s') do s = s - 1 end
  while e <= #line and line:sub(e, e):match('%s') do e = e + 1 end

  if e - s > 1 then
    vim.api.nvim_set_current_line(line:sub(1, s - 1) .. ' ' .. line:sub(e))
    vim.fn.cursor(0, s)
  end
end
vim.keymap.set('n', '<leader><Space>', just_one_space, { noremap = true, silent = true, desc = 'Just one space' })

-- Always use vertical diff splits.
vim.opt.diffopt:append("vertical")

-- double tap esc to unset highlighting
vim.keymap.set('n', '<Esc><Esc>', ':noh<CR>', { noremap = true, silent = true })

-- https://stackoverflow.com/a/2288438/1745064
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Display whitespace characters, including tabs.
vim.opt.list = true
vim.opt.listchars = "tab:|\\ ,trail:·,nbsp:·"

-- Disable fold.
vim.opt.foldenable = false

-- Disable --INSERT-- which is not necessary, because:
--   Neovim changes cursor shape automatically.
--   lualine.nvim already displays it.
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
  -- Syntax highlighting
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require('nvim-treesitter').setup({
        ensure_installed = { 'lua', 'vim', 'vimdoc', 'bash', 'python', 'json', 'yaml', 'markdown' },
        auto_install = true,
      })
    end,
  },

  -- Linting and fixing
  'dense-analysis/ale',

  -- Git integration
  'mhinz/vim-signify',
  'tpope/vim-fugitive',

  -- Editing
  {
    'kylechui/nvim-surround',
    config = function() require('nvim-surround').setup() end,
  },
  { 'junegunn/vim-easy-align', cmd = { 'EasyAlign' } },

  -- Navigation and search
  'nvim-lua/plenary.nvim',
  {
    'nvim-telescope/telescope.nvim',
    config = function()
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Find files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Buffers' })
    end,
  },
  'junegunn/vim-slash',

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
  {
    'nvim-lualine/lualine.nvim',
    config = function() require('lualine').setup({ options = { theme = 'onedark' } }) end,
  },
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    config = function() require('which-key').setup() end,
  },

  -- Better defaults
  'tpope/vim-unimpaired',

  -- Color schemes
  { 'joshdick/onedark.vim', name = 'onedark' },

  -- Diff tools
  'whiteinge/diffconflicts',
}

require("lazy").setup(plugins)

-- Configure ale
vim.g.ale_lint_on_enter = 0
vim.g.ale_virtualtext_cursor = 0

-- Configure vim-fugitive custom commands
vim.cmd("command! Gadd Gwrite")
vim.cmd("command! Gwqall Gwq")

-- Configure vim-eunuch custom command
vim.cmd("command! DoasWrite execute 'silent! write !doas tee % >/dev/null' <bar> edit!")

-- Clipboard integration - use Neovim's built-in OSC52
vim.g.clipboard = {
  name = 'OSC 52',
  copy = {
    ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
    ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
  },
  paste = {
    ['+'] = require('vim.ui.clipboard.osc52').paste('+'),
    ['*'] = require('vim.ui.clipboard.osc52').paste('*'),
  },
}
vim.keymap.set('v', '<C-c>', '"+y', { noremap = true, silent = true })

-- Set color theme / scheme
pcall(function() vim.cmd("colorscheme onedark") end)

-- Highlight yanked text (built-in replacement for vim-highlightedyank)
vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
  callback = function() vim.highlight.on_yank() end,
})
