require('nvim-tree').setup {}

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', 'tc', '<cmd> NvimTreeFindFileToggle<CR>', opts)
