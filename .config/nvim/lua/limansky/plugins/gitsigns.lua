return {
  'lewis6991/gitsigns.nvim', 
  dependencies = { 'nvim-lua/plenary.nvim' },
  event = { "BufReadPre", "BufNewFile" },
  opts = {
    on_attach = function(bufnr)

      local gs = package.loaded.gitsigns

      local function map(mode, l, r, desc)
        vim.keymap.set(mode, l, r, { buffer = bufnr, desc = desc })
      end

      map('n', '<leader>hs', gs.stage_hunk, "Stage hunk")
      map('n', '<leader>hr', gs.reset_hunk, "Reset hunk")
      map('n', '<leader>hu', gs.undo_stage_hunk, "Undo stage hunk")
      map('n', '<leader>hp', gs.preview_hunk, "Preview hunk")
      map('n', '<leader>lb', gs.toggle_current_line_blame, "Blame current line")
      map('n', '<leader>gb', gs.blame, "Git blame")
      map("n", "<leader>hd", gs.diffthis, "Diff this")
    end
  }

}
