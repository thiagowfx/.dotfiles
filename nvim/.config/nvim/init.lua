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
    event = 'VeryLazy',
    build = ':TSUpdate',
    opts = {
      ensure_installed = { 'lua', 'vim', 'vimdoc', 'bash', 'python', 'json', 'yaml', 'markdown' },
      auto_install = true,
    },
  },

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
    config = function() require('nvim-autopairs').setup() end,
  },
  { 'junegunn/vim-easy-align', cmd = { 'EasyAlign' } },

  -- Navigation and search
  'nvim-lua/plenary.nvim',
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
      require('oil').setup()
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
    config = function() require('todo-comments').setup() end,
  },

  -- UI enhancements
  {
    'nvim-lualine/lualine.nvim',
    config = function() require('lualine').setup({ options = { theme = 'onedark' } }) end,
  },
  {
    'lukas-reineke/indent-blankline.nvim',
    main = 'ibl',
    config = function() require('ibl').setup() end,
  },
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    opts = {
      spec = {
        { '<leader>', group = 'Leader' },
      },
    },
  },

  -- Better defaults
  'tpope/vim-unimpaired',

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

  -- LSP, Linting, and Formatting
  'neovim/nvim-lspconfig',
  {
    'mfussenegger/nvim-lint',
    config = function()
      require('lint').linters_by_ft = {
        sh = { 'shellcheck' },
        bash = { 'shellcheck' },
        yaml = { 'yamllint' },
        json = { 'jsonlint' },
      }
      vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufReadPost' }, {
        callback = function() require('lint').try_lint() end,
      })
    end,
  },
  {
    'stevearc/conform.nvim',
    opts = {
      formatters_by_ft = {
        lua = { 'stylua' },
        python = { 'black' },
        sh = { 'shfmt' },
        bash = { 'shfmt' },
        yaml = { 'yamlfmt' },
        json = { 'jq' },
      },
    },
    keys = {
      {
        '<leader>f',
        function() require('conform').format({ async = true, lsp_fallback = true }) end,
        desc = 'Format buffer',
      },
    },
  },
}

require("lazy").setup(plugins)

-- LSP configuration (Neovim 0.11+ native API)
vim.lsp.config.lua_ls = {
  settings = {
    Lua = {
      runtime = { version = 'LuaJIT' },
      diagnostics = { globals = { 'vim' } },
      workspace = { library = vim.api.nvim_get_runtime_file('', true), checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}
vim.lsp.config.pyright = {}
vim.lsp.config.bashls = {}
vim.lsp.config.yamlls = {}
vim.lsp.config.jsonls = {}
vim.lsp.enable({ 'lua_ls', 'pyright', 'bashls', 'yamlls', 'jsonls' })

-- Configure LSP diagnostics display
vim.diagnostic.config({
  virtual_text = {
    prefix = '●',
    spacing = 2,
  },
  signs = true,
  underline = true,
  update_in_insert = false,
})

vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', { clear = true }),
  callback = function(ev)
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, opts)
  end,
})

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
  callback = function() vim.highlight.on_yank() end,
})

-- Auto-create directories when saving (replaces vim-mkdir)
vim.api.nvim_create_autocmd('BufWritePre', {
  group = vim.api.nvim_create_augroup('AutoMkdir', { clear = true }),
  callback = function(event)
    local file = vim.uv.fs_realpath(event.match) or event.match
    vim.fn.mkdir(vim.fn.fnamemodify(file, ':p:h'), 'p')
  end,
})
