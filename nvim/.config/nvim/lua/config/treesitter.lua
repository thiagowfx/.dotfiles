-- Treesitter plugin spec
return {
  'nvim-treesitter/nvim-treesitter',
  event = 'VeryLazy',
  build = ':TSUpdate',
  opts = {
    ensure_installed = {
      -- keep-sorted start
      'bash',
      'go',
      'json',
      'lua',
      'markdown',
      'python',
      'vim',
      'vimdoc',
      'yaml',
      -- keep-sorted end
    },
    auto_install = true,
  },
}
