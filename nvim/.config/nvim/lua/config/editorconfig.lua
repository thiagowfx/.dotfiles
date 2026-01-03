local M = {}

function M.get_max_line_length(file)
  -- Skip processing for non-real files (e.g., fugitive:// buffers)
  if not file or file == '' or file:match('^fugitive://') then
    return nil
  end

  local dir = vim.fn.fnamemodify(file, ':h')
  local editorconfig_path = vim.fn.findfile('.editorconfig', dir .. ';')

  if editorconfig_path == '' then
    return nil
  end

  local lines = vim.fn.readfile(editorconfig_path)
  local in_md_section = false

  for _, line in ipairs(lines) do
    if line:match('^%[%*%.md%]') then
      in_md_section = true
    elseif line:match('^%[') then
      in_md_section = false
    end

    if in_md_section then
      local value = line:match('^max_line_length%s*=%s*(%d+)')
      if value then
        return value
      end
    end
  end

  return nil
end

return M
