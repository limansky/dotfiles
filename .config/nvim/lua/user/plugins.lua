local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

return require('lazy').setup({

  -- interface
  'nvim-tree/nvim-web-devicons',
  { 'nvim-lualine/lualine.nvim', dependencies = {'nvim-tree/nvim-web-devicons' } },
  {'akinsho/bufferline.nvim', dependencies = 'nvim-tree/nvim-web-devicons' },
  { 'nvim-tree/nvim-tree.lua', dependencies = { 'nvim-tree/nvim-web-devicons' } },
  { 'nvim-telescope/telescope.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },
  { 'folke/trouble.nvim', dependencies = { 'nvim-tree/nvim-web-devicons' } },
  { 'nvim-lua/popup.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },
  'lukas-reineke/indent-blankline.nvim',

  -- typing
  'windwp/nvim-autopairs',

  -- languages
  'lervag/vimtex',
  {'scalameta/nvim-metals', dependencies = { 'nvim-lua/plenary.nvim' } },

  -- Treesitter
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },

  'numToStr/Comment.nvim',
  'JoosepAlviste/nvim-ts-context-commentstring',
  'windwp/nvim-ts-autotag',

  -- LSP
  'williamboman/mason.nvim',
  'williamboman/mason-lspconfig.nvim',
  {
    'neovim/nvim-lspconfig',
    lazy = false,
    dependencies = {
      'mason.nvim',
      'williamboman/mason-lspconfig.nvim'
    }
  }, -- enable LSP
  'hrsh7th/nvim-cmp', -- Autocompletion plugin
  'hrsh7th/cmp-nvim-lsp', -- LSP source for nvim-cmp
  'saadparwaiz1/cmp_luasnip', -- Snippets source for nvim-cmp
  'L3MON4D3/LuaSnip', -- Snippets plugin

  -- git
  { 'lewis6991/gitsigns.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },
  { 'sindrets/diffview.nvim', dependencies = { 'nvim-lua/plenary.nvim' } },

  -- colorschemes
  -- 'doums/darcula',
  'rebelot/kanagawa.nvim'
})
