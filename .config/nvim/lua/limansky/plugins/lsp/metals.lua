return {
  'scalameta/nvim-metals',
  dependencies = {
    'nvim-lua/plenary.nvim',
    { "j-hui/fidget.nvim", opts = {} }
  },
  ft = { 'scala', 'sbt', 'java' },
  opts = function()
    local mc = require('metals').bare_config()

    mc.settings = {
      showInferredType = true,
      showImplicitArguments = true
    }

    mc.capabilities = require('cmp_nvim_lsp').default_capabilities()

    -- TODO: Setup DAP
    return mc
  end,
  config = function(self, metals_config)
    local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
    vim.api.nvim_create_autocmd("FileType", {
      pattern = self.ft,
      callback = function()
        require("metals").initialize_or_attach(metals_config)
      end,
      group = nvim_metals_group,
    })
  end
}
