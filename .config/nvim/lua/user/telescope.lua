require('telescope').setup()

local opts = { noremap = true, silent = true }

local tele = require('telescope.builtin')

vim.keymap.set('n', '<leader>fg', tele.live_grep, opts)
vim.keymap.set('n', '<leader>ff', tele.find_files, opts)
vim.keymap.set('n', '<leader>fh', tele.help_tags, opts)
vim.keymap.set('n', '<leader>fc', tele.git_bcommits, opts)
