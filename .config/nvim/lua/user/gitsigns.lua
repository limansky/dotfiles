require('gitsigns').setup {
  on_attach = function(bufnr)

    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    map('n', '<leader>hs', gs.stage_hunk)
    map('n', '<leader>hr', gs.reset_hunk)
    map('n', '<leader>hu', gs.undo_stage_hunk)
    map('n', '<leader>hp', gs.preview_hunk)
    map('n', '<leader>lb', gs.toggle_current_line_blame)
  end
}
