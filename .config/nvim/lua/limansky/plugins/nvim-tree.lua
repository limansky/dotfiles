return {
  "nvim-tree/nvim-tree.lua",
  dependencies = "nvim-tree/nvim-web-devicons",
  config = function()
    local nvtree = require('nvim-tree')

    vim.g.loaded_netrw = 1
    vim.g.loaded_netrwPlugin = 1

    nvtree.setup {}

    local keymap = vim.keymap

    keymap.set('n', 'tc', '<cmd>NvimTreeFindFileToggle<CR>', { desc = 'Toggle current file in file explorer' })
  end
}
