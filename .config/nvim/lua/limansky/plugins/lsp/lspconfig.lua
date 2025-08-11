return {
  'neovim/nvim-lspconfig',
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    'hrsh7th/cmp-nvim-lsp', -- LSP source for nvim-cmp
    { "antosha417/nvim-lsp-file-operations", config = true },
  },
  config = function()
    local cmp_nvim_lsp = require('cmp_nvim_lsp')

    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspConfig", {}),
      callback = function(ev)
        local map = function(m, k, a, d)
          local o = { buffer = ev.buf, silent = true, noremap = true, desc = d }
          vim.keymap.set(m, k, a, o)
        end

        map('n', 'gd', vim.lsp.buf.definition, 'Go to definition')
        map('n', 'gi', vim.lsp.buf.implementation, 'Go to implementation')
        map('n', '<leader>lf', vim.lsp.buf.format, 'Reformat the code')
      end
    })

    vim.diagnostic.config({
      signs = {
        active = true,
        text = {
          [vim.diagnostic.severity.ERROR] = "",
          [vim.diagnostic.severity.WARN]  = "",
          [vim.diagnostic.severity.HINT]  = "󰟃",
          [vim.diagnostic.severity.INFO]  = ""
        }
      }
    })

    local capabilities = cmp_nvim_lsp.default_capabilities()

    vim.lsp.config('*', {
      capabilities = capabilities
    })

    vim.lsp.config('lua_ls', {
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim' }
          }
        }
      }
    })
  end
}
