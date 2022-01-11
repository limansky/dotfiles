require('telescope').setup()

local opts = { noremap = true, silent = true }

vim.api.nvim_set_keymap('n', '<leader>fg', [[<cmd>lua require("telescope.builtin").live_grep()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>lua require("telescope.builtin").find_files()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>fh', [[<cmd>lua require("telescope.builtin").help_tags()<CR>]], opts)
vim.api.nvim_set_keymap('n', '<leader>fc', [[<cmd>lua require("telescope.builtin").git_bcommits()<CR>]], opts)
