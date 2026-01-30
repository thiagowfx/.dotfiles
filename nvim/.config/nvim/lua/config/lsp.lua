-- LSP, Linting, and Formatting plugins
local plugins = {
  {
    'williamboman/mason.nvim',
    build = ':MasonUpdate',
    config = function()
      require('mason').setup()
    end,
  },
  {
    'williamboman/mason-lspconfig.nvim',
    dependencies = { 'mason.nvim' },
    config = function()
      require('mason-lspconfig').setup({
        automatic_installation = true,
        ensure_installed = {
          -- keep-sorted start
          'bashls',
          'clangd',
          'dockerls',
          'gopls',
          'jsonls',
          'lua_ls',
          'marksman',
          'pyright',
          'terraformls',
          'ts_ls',
          'yamlls',
          -- keep-sorted end
        },
      })
    end,
  },
  'neovim/nvim-lspconfig',
  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-path',
    },
    config = function()
      local cmp = require('cmp')
      cmp.setup({
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }),
        }),
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
        }, {
          { name = 'buffer' },
          { name = 'path' },
        }),
      })
      local cmp_enabled = true
      vim.keymap.set('n', '<leader>da', function()
        cmp_enabled = not cmp_enabled
        cmp.setup.buffer({ enabled = cmp_enabled })
        print('Auto-completion ' .. (cmp_enabled and 'enabled' or 'disabled'))
      end, { desc = 'Toggle auto-completion' })
    end,
  },
  {
    'mfussenegger/nvim-lint',
    config = function()
      require('lint').linters_by_ft = {
        bash = { 'shellcheck' },
        c = { 'clang-tidy' },
        cpp = { 'clang-tidy' },
        dockerfile = { 'hadolint' },
        go = { 'golangci-lint' },
        json = { 'jsonlint' },
        python = { 'ruff' },
        ruby = { 'rubocop' },
        sh = { 'shellcheck' },
        terraform = { 'tflint' },
        yaml = { 'yamllint' },
        zsh = { 'shellcheck' },
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
        bash = { 'shfmt' },
        c = { 'clang-format' },
        cpp = { 'clang-format' },
        go = { 'gofmt' },
        json = { 'jq' },
        lua = { 'stylua' },
        python = { 'black' },
        ruby = { 'rubocop' },
        sh = { 'shfmt' },
        terraform = { 'terraform_fmt' },
        yaml = { 'yamlfmt' },
        zsh = { 'shfmt' },
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
  {
    'stevearc/aerial.nvim',
    keys = {
      { '<leader>a', '<cmd>AerialToggle<CR>', desc = 'Toggle outline' },
      { '<leader>do', '<cmd>AerialToggle<CR>', desc = 'Toggle outline' },
    },
    config = function()
      require('aerial').setup({
        layout = { default_direction = 'right', width = 35 },
        filter_kind = false,
      })
      vim.keymap.set('n', '[a', '<cmd>AerialPrev<CR>', { desc = 'Previous symbol' })
      vim.keymap.set('n', ']a', '<cmd>AerialNext<CR>', { desc = 'Next symbol' })
    end,
  },
  {
    'folke/trouble.nvim',
    dependencies = { 'echasnovski/mini.icons' },
    keys = {
      { '<leader>xx', '<cmd>Trouble diagnostics toggle<CR>', desc = 'Diagnostics (Trouble)' },
      { '<leader>xd', '<cmd>Trouble diagnostics toggle filter.buf=0<CR>', desc = 'Buffer diagnostics (Trouble)' },
      { '<leader>xq', '<cmd>Trouble qflist toggle<CR>', desc = 'Quickfix (Trouble)' },
      { '<leader>xl', '<cmd>Trouble loclist toggle<CR>', desc = 'Location list (Trouble)' },
    },
    opts = {},
  },
}

-- LSP configuration function (called after lazy.setup)
local function setup()
  -- Store original diagnostic config for toggling
  local diagnostic_config = {
    virtual_text = { prefix = '●', spacing = 2 },
    signs = true,
    underline = true,
    update_in_insert = false,
  }
  local diagnostics_enabled = true

  -- Configure signature help display
  vim.lsp.handlers['textDocument/signatureHelp'] = vim.lsp.with(
    vim.lsp.handlers.signature_help,
    { border = 'rounded' }
  )

  -- Configure LSP servers (mason-lspconfig handles automatic setup)
  -- Get capabilities from nvim-cmp
  local cmp_ok, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
  local capabilities = cmp_ok and cmp_nvim_lsp.default_capabilities() or {}

  -- Configure lua_ls
  vim.lsp.config.lua_ls = {
    capabilities = capabilities,
    settings = {
      Lua = {
        runtime = { version = 'LuaJIT' },
        diagnostics = { globals = { 'vim' } },
        workspace = { library = vim.api.nvim_get_runtime_file('', true), checkThirdParty = false },
        telemetry = { enable = false },
      },
    },
  }

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

  -- Set up LSP keymaps on attach
  vim.api.nvim_create_autocmd('LspAttach', {
    group = vim.api.nvim_create_augroup('UserLspConfig', { clear = true }),
    callback = function(ev)
      local buf = ev.buf
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, { buffer = buf, desc = 'Go to definition' })
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, { buffer = buf, desc = 'Go to declaration' })
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, { buffer = buf, desc = 'Go to references' })
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, { buffer = buf, desc = 'Go to implementation' })
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, { buffer = buf, desc = 'Hover documentation' })
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, { buffer = buf, desc = 'Signature help' })
      vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, { buffer = buf, desc = 'Signature help' })
      vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, { buffer = buf, desc = 'Rename symbol' })
      vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, { buffer = buf, desc = 'Code action' })
      vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { buffer = buf, desc = 'Previous diagnostic' })
      vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { buffer = buf, desc = 'Next diagnostic' })
      vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { buffer = buf, desc = 'Show diagnostics' })
      vim.keymap.set('n', '<leader>dd', function()
        diagnostics_enabled = not diagnostics_enabled
        if diagnostics_enabled then
          vim.diagnostic.config(diagnostic_config)
        else
          vim.diagnostic.config({
            virtual_text = false,
            signs = false,
            underline = false,
          })
        end
      end, { desc = 'Toggle diagnostics' })

      -- Auto-trigger signature help on ( and , (with small delay for LSP to catch up)
      vim.api.nvim_create_autocmd('InsertCharPre', {
        buffer = ev.buf,
        callback = function()
          if vim.v.char == '(' or vim.v.char == ',' then
            vim.defer_fn(function()
              -- Check if any attached LSP supports signature help
              local clients = vim.lsp.get_clients({ bufnr = ev.buf })
              for _, client in ipairs(clients) do
                if client.supports_method('textDocument/signatureHelp') then
                  vim.lsp.buf.signature_help()
                  return
                end
              end
            end, 50)
          end
        end,
      })
    end,
  })
end

return { plugins = plugins, setup = setup }
