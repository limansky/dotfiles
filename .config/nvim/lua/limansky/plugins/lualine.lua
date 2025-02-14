local function metals_status()
  return vim.g['metals_status'] or ''
end

return {
  'nvim-lualine/lualine.nvim',
  dependencies = {'nvim-tree/nvim-web-devicons' },
  config = function()
    local lualine = require('lualine')
    lualine.setup {
      sections = {
        lualine_c = { 'filename', metals_status }
      }
    }
  end
}
