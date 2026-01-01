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

-- Enable persistent undo (uses XDG_STATE_HOME/nvim/undo by default).
vim.opt.undofile = true

-- Auto-reload files changed on disk
vim.opt.autoread = true
vim.api.nvim_create_autocmd({ 'FocusGained', 'BufEnter' }, {
  command = 'checktime',
})

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
    "--depth=1",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugin specs
local plugins = {
  -- Syntax highlighting
  pcall(require, 'config.treesitter') and require('config.treesitter') or nil,

  -- Git integration
  {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup({
        on_attach = function(bufnr)
          local gs = require('gitsigns')
          -- ]c / [c: next/prev hunk (falls back to default in diff mode)
          vim.keymap.set('n', ']c', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.next_hunk() end)
            return '<Ignore>'
          end, { expr = true, buffer = bufnr, desc = 'Next hunk' })
          vim.keymap.set('n', '[c', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.prev_hunk() end)
            return '<Ignore>'
          end, { expr = true, buffer = bufnr, desc = 'Prev hunk' })
          vim.keymap.set('n', '<leader>s', gs.stage_hunk, { buffer = bufnr, desc = 'Stage hunk' })
          vim.keymap.set('n', '<leader>u', gs.undo_stage_hunk, { buffer = bufnr, desc = 'Unstage hunk' })
        end,
      })
    end,
  },
  'tpope/vim-fugitive',

  -- Editing
  {
    'kylechui/nvim-surround',
    config = function() require('nvim-surround').setup() end,
  },
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    config = function() require('nvim-autopairs').setup({ map_cr = false }) end,
  },
  { 'junegunn/vim-easy-align', cmd = { 'EasyAlign' } },

  -- Navigation and search
  {
    'nvim-telescope/telescope.nvim',
    config = function()
      local builtin = require('telescope.builtin')
      vim.keymap.set('n', '<leader>ff', function()
        builtin.find_files({ hidden = true, respect_gitignore = true })
      end, { desc = 'Find files' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Live grep' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Buffers' })
    end,
  },
  'junegunn/vim-slash',

  -- File and buffer management
  {
    'stevearc/oil.nvim',
    config = function()
      require('oil').setup({ view_options = { show_hidden = true } })
      vim.keymap.set('n', '-', '<cmd>Oil<CR>', { desc = 'Open parent directory' })
    end,
  },
  'aymericbeaumet/vim-symlink',
  'bogado/file-line',
  'tpope/vim-eunuch',

  -- Code quality and formatting
  'tpope/vim-endwise',
  {
    'echasnovski/mini.trailspace',
    config = function()
      require('mini.trailspace').setup()
      vim.api.nvim_create_user_command('FixWhitespace', function()
        MiniTrailspace.trim()
      end, {})
    end,
  },
  {
    'folke/todo-comments.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {},
  },

  -- UI enhancements
  {
    'nvim-lualine/lualine.nvim',
    config = function() require('lualine').setup({ options = { theme = 'onedark' } }) end,
  },
  {
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    opts = {},
  },
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    dependencies = { 'echasnovski/mini.icons' },
    opts = {
      delay = 2000,
      spec = {
        { '<leader>', group = 'Leader' },
      },
    },
  },

  -- Better defaults
  'tpope/vim-unimpaired',
  {
    'ethanholz/nvim-lastplace',
    config = function()
      local ok, lp = pcall(require, 'nvim-lastplace')
      if ok then
        pcall(lp.setup)
      end
    end,
  },

  -- Color schemes
  {
    'navarasu/onedark.nvim',
    priority = 1000,
    config = function()
      require('onedark').setup({ style = 'dark' })
      require('onedark').load()
    end,
  },

  -- Diff tools
  'whiteinge/diffconflicts',
}

-- Load LSP, Linting, and Formatting plugins
local lsp = pcall(require, 'config.lsp') and require('config.lsp') or { plugins = {}, setup = function() end }
for _, plugin in ipairs(lsp.plugins) do
  table.insert(plugins, plugin)
end

require("lazy").setup(plugins)

-- Run LSP setup after plugins are loaded
lsp.setup()

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

-- Highlight yanked text (built-in replacement for vim-highlightedyank)
vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
  callback = function() vim.hl.on_yank() end,
})

-- Set colorcolumn for markdown files from .editorconfig e.g. for blog posts
local editorconfig = require('config.editorconfig')
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'markdown',
  callback = function(event)
    local cc = editorconfig.get_max_line_length(event.file)
    if cc then
      vim.opt_local.colorcolumn = cc
    end
  end,
})

-- Auto-create directories when saving (replaces vim-mkdir)
vim.api.nvim_create_autocmd('BufWritePre', {
  group = vim.api.nvim_create_augroup('AutoMkdir', { clear = true }),
  callback = function(event)
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
  end,
})
