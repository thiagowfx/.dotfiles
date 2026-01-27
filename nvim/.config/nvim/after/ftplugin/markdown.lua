-- Set colorcolumn from .editorconfig e.g. for blog posts
local editorconfig = require('config.editorconfig')
local cc = editorconfig.get_max_line_length(vim.fn.expand('%:p'))
if cc then
  vim.opt_local.colorcolumn = cc
end
