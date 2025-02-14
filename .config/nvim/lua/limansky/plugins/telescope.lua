return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local telescope = require('telescope')
    local tele = require('telescope.builtin')

    telescope.setup({})

    local function map(m, k, a, d)
      vim.keymap.set(m, k, a, { desc = d })
    end

    map('n', '<leader>fg', tele.live_grep, "Live grep")
    map('n', '<leader>ff', tele.find_files, "Find file")
    map('n', '<leader>fh', tele.help_tags, "Find help tag")
    map('n', '<leader>fc', tele.git_bcommits, "Find commit")
  end
}
