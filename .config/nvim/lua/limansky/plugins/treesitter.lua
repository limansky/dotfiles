return {
  "nvim-treesitter/nvim-treesitter",
  event = { "BufReadPre", "BufNewFile" },
  build = ":TSUpdate",
  dependencies = {
    "windwp/nvim-ts-autotag",
  },
  config = function()
    local treesitter = require('nvim-treesitter.configs')

    treesitter.setup({
      -- context_commentstring = {
      --   enable = true,
      --   enable_autocmd = false,
      -- },
      highlight = {
        enable = true,
        disable = { 'scala' }
      },
      autopairs = {
        enable = true,
      },
      autotag = {
        enable = true,
      },
      ensure_installed = {
        'lua',
        'typescript',
        'javascript',
        'tsx',
        'yaml',
        'html',
        'css',
        'gitignore',
        'dockerfile'
      }
    })
  end
}
