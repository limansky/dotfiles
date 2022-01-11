require('nvim-treesitter.configs').setup {
  context_commentstring = {
    enable = true,
    enable_autocmd = false,
  },
  highlight = {
    enable = true,
    disable = { 'scala' }
  },
  autopairs = {
    enable = true,
  },
  autotag = {
    enable = true,
  }
}
