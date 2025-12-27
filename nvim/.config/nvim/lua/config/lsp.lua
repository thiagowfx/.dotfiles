-- Helper function to check if executable exists
local function has_executable(name)
  return vim.fn.executable(name) == 1
end

-- LSP, Linting, and Formatting plugins
local plugins = {
  'neovim/nvim-lspconfig',
  {
    'mfussenegger/nvim-lint',
    config = function()
      local linters_by_ft = {}
      if has_executable('shellcheck') then
        linters_by_ft.bash = { 'shellcheck' }
        linters_by_ft.sh = { 'shellcheck' }
        linters_by_ft.zsh = { 'shellcheck' }
      end
      if has_executable('clang-tidy') then
        linters_by_ft.c = { 'clang-tidy' }
        linters_by_ft.cpp = { 'clang-tidy' }
      end
      if has_executable('golangci-lint') then
        linters_by_ft.go = { 'golangci-lint' }
      end
      if has_executable('jsonlint') then
        linters_by_ft.json = { 'jsonlint' }
      end
      local python_linters = {}
      if has_executable('mypy') then
        table.insert(python_linters, 'mypy')
      end
      if has_executable('ruff') then
        table.insert(python_linters, 'ruff')
      end
      if #python_linters > 0 then
        linters_by_ft.python = python_linters
      end
      if has_executable('rubocop') then
        linters_by_ft.ruby = { 'rubocop' }
      end
      if has_executable('yamllint') then
        linters_by_ft.yaml = { 'yamllint' }
      end
      require('lint').linters_by_ft = linters_by_ft
      vim.api.nvim_create_autocmd({ 'BufWritePost', 'BufReadPost' }, {
        callback = function() require('lint').try_lint() end,
      })
    end,
  },
  {
    'stevearc/conform.nvim',
    opts = {
      formatters_by_ft = function()
        local formatters = {}
        if has_executable('shfmt') then
          formatters.bash = { 'shfmt' }
          formatters.sh = { 'shfmt' }
          formatters.zsh = { 'shfmt' }
        end
        if has_executable('clang-format') then
          formatters.c = { 'clang-format' }
          formatters.cpp = { 'clang-format' }
        end
        if has_executable('gofmt') then
          formatters.go = { 'gofmt' }
        end
        if has_executable('jq') then
          formatters.json = { 'jq' }
        end
        if has_executable('stylua') then
          formatters.lua = { 'stylua' }
        end
        if has_executable('black') then
          formatters.python = { 'black' }
        end
        if has_executable('rubocop') then
          formatters.ruby = { 'rubocop' }
        end
        if has_executable('yamlfmt') then
          formatters.yaml = { 'yamlfmt' }
        end
        return formatters
      end,
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
  local servers = {}

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
  table.insert(servers, 'lua_ls')

  if has_executable('clangd') then
    vim.lsp.config.clangd = {}
    table.insert(servers, 'clangd')
  end

  if has_executable('gopls') then
    vim.lsp.config.gopls = {}
    table.insert(servers, 'gopls')
  end

  if has_executable('pyright') then
    vim.lsp.config.pyright = {}
    table.insert(servers, 'pyright')
  end

  if has_executable('bash-language-server') then
    vim.lsp.config.bashls = {}
    table.insert(servers, 'bashls')
  end

  if has_executable('yaml-language-server') then
    vim.lsp.config.yamlls = {}
    table.insert(servers, 'yamlls')
  end

  if has_executable('vscode-json-language-server') then
    vim.lsp.config.jsonls = {}
    table.insert(servers, 'jsonls')
  end

  if has_executable('marksman') then
    vim.lsp.config.marksman = {}
    table.insert(servers, 'marksman')
  end

  vim.lsp.enable(servers)

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
