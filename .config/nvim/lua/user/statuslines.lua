local function metalsStatus()
  return vim.g['metals_status'] or ""
end

require('lualine').setup {
  sections = {
    lualine_c = { 'filename', metalsStatus }
  }
}

require('bufferline').setup{}
