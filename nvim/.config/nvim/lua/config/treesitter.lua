-- Treesitter plugin spec
return {
  'nvim-treesitter/nvim-treesitter',
  event = 'VeryLazy',
  build = ':TSUpdate',
  opts = {
    ensure_installed = {
      -- keep-sorted start
      'bash',
      'c',
      'cpp',
      'go',
      'json',
      'lua',
      'markdown',
      'python',
      'ruby',
      'vim',
      'vimdoc',
      'yaml',
      'zsh',
      -- keep-sorted end
    },
    auto_install = true,
  },
}
