-- LSP, Linting, and Formatting plugins
local plugins = {
  'neovim/nvim-lspconfig',
  {
    'mfussenegger/nvim-lint',
    config = function()
      require('lint').linters_by_ft = {
        -- keep-sorted start
        bash = { 'shellcheck' },
        c = { 'clang-tidy' },
        cpp = { 'clang-tidy' },
        go = { 'golangci-lint' },
        json = { 'jsonlint' },
        python = { 'ruff' },
        sh = { 'shellcheck' },
        yaml = { 'yamllint' },
        -- keep-sorted end
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
        -- keep-sorted start
        bash = { 'shfmt' },
        c = { 'clang-format' },
        cpp = { 'clang-format' },
        go = { 'gofmt' },
        json = { 'jq' },
        lua = { 'stylua' },
        python = { 'black' },
        sh = { 'shfmt' },
        yaml = { 'yamlfmt' },
        -- keep-sorted end
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

-- LSP configuration function (called after lazy.setup)
local function setup()
  -- Configure LSP servers (Neovim 0.11+ native API)
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
  vim.lsp.config.clangd = {}
  vim.lsp.config.gopls = {}
  vim.lsp.config.pyright = {}
  vim.lsp.config.bashls = {}
  vim.lsp.config.yamlls = {}
  vim.lsp.config.jsonls = {}
  vim.lsp.enable({ 'lua_ls', 'clangd', 'gopls', 'pyright', 'bashls', 'yamlls', 'jsonls' })

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
end

return { plugins = plugins, setup = setup }
