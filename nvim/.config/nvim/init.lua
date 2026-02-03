-- Neovim config file

-- Plugin management
--   :Lazy
--
-- Keymaps
--   :map
--
-- Disabling Features (one-off, for the current buffer):
--   <leader>da - Toggle auto-completion (off by default)
--   <leader>dc - Toggle Copilot, or :Copilot enable (off by default)
--   <leader>dd - Toggle diagnostics (on by default)
--   <leader>do - Toggle outline (aerial.nvim)
--   <leader>ds - Toggle spell checking, or :set spell (off by default)
--
-- Custom Keybindings:
--   Leader: comma (,)
--
--   Editing:
--     J         - Join lines without adding space (built-in)
--     Q         - Reflow current paragraph (built-in gwip)
--     W         - Format current paragraph with ipfmt (external command)
--     <leader>w - Fix whitespace (mini.trailspace)
--     <leader><Space> - Just one space (custom function)
--
--   Git:
--     ]c        - Next git hunk (gitsigns.nvim)
--     [c        - Previous git hunk (gitsigns.nvim)
--     <leader>s - Stage current hunk (gitsigns.nvim)
--     <leader>u - Unstage current hunk (gitsigns.nvim)
--
--   LSP (Language Server Protocol):
--     ]d        - Next diagnostic (nvim-lspconfig)
--     [d        - Previous diagnostic (nvim-lspconfig)
--     gd        - Go to definition (nvim-lspconfig)
--     gD        - Go to declaration (nvim-lspconfig)
--     gr        - Go to references (nvim-lspconfig)
--     gi        - Go to implementation (nvim-lspconfig)
--     K         - Hover documentation (nvim-lspconfig)
--     <C-k>     - Signature help (nvim-lspconfig)
--     <leader>rn - Rename symbol (nvim-lspconfig)
--     <leader>ca - Code actions (nvim-lspconfig)
--     <leader>e - Show diagnostics in float window (nvim-lspconfig)
--     <leader>dd - Toggle diagnostics (nvim-lspconfig)
--     <leader>f - Format buffer (conform.nvim)
--     <leader>a - Toggle outline (aerial.nvim)
--     [a        - Previous symbol (aerial.nvim)
--     ]a        - Next symbol (aerial.nvim)
--     <leader>xx - Diagnostics list (trouble.nvim)
--     <leader>xd - Buffer diagnostics (trouble.nvim)
--     <leader>xq - Quickfix list (trouble.nvim)
--     <leader>xl - Location list (trouble.nvim)
--
--   LSP-Enhanced Completion (nvim-cmp) - off by default, toggle with <leader>da:
--     <C-n>     - Open completion menu (or next item if menu open)
--     <C-p>     - Previous completion item
--     <CR>      - Confirm selection
--     <C-e>     - Abort completion
--     <C-b>     - Scroll docs backward
--     <C-f>     - Scroll docs forward
--     <C-k>     - Signature help (insert mode)
--
--   Navigation and Search:
--     <leader>tf - Find files (telescope.nvim, includes hidden)
--     <leader>tg - Live grep (telescope.nvim)
--     <leader>tb - List buffers (telescope.nvim)
--     -         - Open parent directory (oil.nvim)
--
--   Clipboard:
--     <C-c>     - Copy selection to system clipboard (built-in OSC52)
--
--   AI (GitHub Copilot):
--   First-time setup: :Copilot setup, :Copilot status
--     <leader>dc - Toggle Copilot (disabled by default)
--     <C-j>     - Accept suggestion (insert mode)
--     <M-]>     - Next suggestion (insert mode)
--     <M-[>     - Previous suggestion (insert mode)
--     <C-\>     - Dismiss suggestion (insert mode)

-- Set leader key to comma. Default is '\'.
vim.g.mapleader = ","

-- :w save misspellings
vim.cmd("command! GWQ Gwq")
vim.cmd("command! W w")
vim.cmd("command! WQ wq")
vim.cmd("command! Wq wq")

-- https://vi.stackexchange.com/questions/439/how-to-join-lines-without-producing-a-space
vim.keymap.set('n', 'J', 'gJ', { noremap = true, silent = true, desc = 'Join lines without space' })

-- Fix whitespace with <leader>w
vim.keymap.set('n', '<leader>w', ':FixWhitespace<CR>', { noremap = true, silent = true, desc = 'Fix whitespace' })

-- Reflow current paragraph
vim.keymap.set('n', 'Q', 'gwip', { noremap = true, silent = true, desc = 'Reflow paragraph' })
vim.keymap.set('n', 'W', '!ipfmt<Enter>', { noremap = true, silent = true, desc = 'Format paragraph with ipfmt' })

-- Toggle spell checking
vim.keymap.set('n', '<leader>ds', function()
  vim.opt_local.spell = not vim.opt_local.spell:get()
  print('Spell checking ' .. (vim.opt_local.spell:get() and 'enabled' or 'disabled'))
end, { desc = 'Toggle spell checking' })

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

-- Show line numbers (relative for easier motion counts).
vim.opt.number = true
vim.opt.relativenumber = true

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
  {
    'nvim-treesitter/nvim-treesitter-context',
    opts = { max_lines = 5 },
  },

  -- Git integration
  {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup({
        on_attach = function(bufnr)
          local gs = require('gitsigns')
          -- ]c / [c: next/prev hunk (falls back to default in diff mode)
          -- like git-gutter in vim
          vim.keymap.set('n', ']c', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.nav_hunk('next') end)
            return '<Ignore>'
          end, { expr = true, buffer = bufnr, desc = 'Next hunk' })
          vim.keymap.set('n', '[c', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.nav_hunk('prev') end)
            return '<Ignore>'
          end, { expr = true, buffer = bufnr, desc = 'Prev hunk' })
          vim.keymap.set('n', '<leader>s', gs.stage_hunk, { buffer = bufnr, desc = 'Stage hunk' })
          vim.keymap.set('n', '<leader>u', gs.reset_hunk, { buffer = bufnr, desc = 'Reset hunk' })
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
    cmd = 'Telescope',
    keys = {
      { '<leader>tf', function() require('telescope.builtin').find_files({ hidden = true, respect_gitignore = true }) end, desc = 'Find files' },
      { '<leader>tg', function() require('telescope.builtin').live_grep() end, desc = 'Live grep' },
      { '<leader>tb', function() require('telescope.builtin').buffers() end, desc = 'Buffers' },
    },
  },
  { 'junegunn/vim-slash', event = 'CmdlineEnter' },

  -- File and buffer management
  {
    'stevearc/oil.nvim',
    config = function()
      require('oil').setup({ view_options = { show_hidden = true } })
      vim.keymap.set('n', '-', '<cmd>Oil<CR>', { desc = 'Open parent directory' })
    end,
  },
  { 'aymericbeaumet/vim-symlink', event = 'BufReadPre' },
  { 'bogado/file-line', event = 'BufReadPre' },
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
        { '<leader>d', group = 'Disable/Toggle' },
        { '<leader>t', group = 'Telescope' },
        { '<leader>x', group = 'Trouble' },
      },
    },
  },

  -- Better defaults
  { 'tpope/vim-unimpaired', event = 'VeryLazy' },
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

  -- AI
  {
    "sourcegraph/amp.nvim",
    branch = "main",
    lazy = false,
    opts = { auto_start = true, log_level = "info" },
  },
  {
    "github/copilot.vim",
    cmd = "Copilot",
    keys = {
      { '<leader>dc', function()
        vim.g.copilot_enabled = not vim.g.copilot_enabled
        print('Copilot ' .. (vim.g.copilot_enabled and 'enabled' or 'disabled'))
      end, desc = 'Toggle Copilot' },
    },
    config = function()
      vim.g.copilot_enabled = false
      vim.g.copilot_no_tab_map = true
      vim.keymap.set('i', '<C-j>', 'copilot#Accept("\\<CR>")', { expr = true, replace_keycodes = false, desc = 'Accept Copilot suggestion' })
      vim.keymap.set('i', '<M-]>', '<Plug>(copilot-next)', { desc = 'Next Copilot suggestion' })
      vim.keymap.set('i', '<M-[>', '<Plug>(copilot-previous)', { desc = 'Previous Copilot suggestion' })
      vim.keymap.set('i', '<C-\\>', '<Plug>(copilot-dismiss)', { desc = 'Dismiss Copilot suggestion' })
    end,
  },
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
vim.keymap.set('v', '<C-c>', '"+y', { noremap = true, silent = true, desc = 'Copy to clipboard' })

-- Highlight yanked text (built-in replacement for vim-highlightedyank)
vim.api.nvim_create_autocmd('TextYankPost', {
  group = vim.api.nvim_create_augroup('YankHighlight', { clear = true }),
  callback = function() vim.hl.on_yank() end,
})

-- Alias 'md' filetype to 'markdown' (for :set ft=md)
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'md',
  callback = function() vim.bo.filetype = 'markdown' end,
})

-- Auto-create directories when saving (replaces vim-mkdir)
vim.api.nvim_create_autocmd('BufWritePre', {
  group = vim.api.nvim_create_augroup('AutoMkdir', { clear = true }),
  callback = function(event)
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
  end,
})
