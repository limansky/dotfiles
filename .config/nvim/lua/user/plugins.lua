return require('packer').startup(function(use)
  -- packer itself
  use 'wbthomason/packer.nvim'

  -- interface
  use 'nvim-tree/nvim-web-devicons'
  use { 'nvim-lualine/lualine.nvim', requires = {'kyazdani42/nvim-web-devicons', opt = true} }
  use {'akinsho/bufferline.nvim', requires = 'kyazdani42/nvim-web-devicons'}
  use { 'kyazdani42/nvim-tree.lua', requires = { 'kyazdani42/nvim-web-devicons' } }
  use { 'nvim-telescope/telescope.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'folke/trouble.nvim', requires = { 'kyazdani42/nvim-web-devicons' } }
  use { 'nvim-lua/popup.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'lukas-reineke/indent-blankline.nvim'}

  -- typing
  use { 'windwp/nvim-autopairs' }

  -- languages
  use 'lervag/vimtex'
  use {'scalameta/nvim-metals', requires = { 'nvim-lua/plenary.nvim' } }

  -- Treesitter
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  use 'numToStr/Comment.nvim'
  use 'JoosepAlviste/nvim-ts-context-commentstring'
  use 'windwp/nvim-ts-autotag'

  -- LSP
  use 'neovim/nvim-lspconfig' -- enable LSP
  use 'williamboman/nvim-lsp-installer' -- install LSP servers
  use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
  use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp
  use 'saadparwaiz1/cmp_luasnip' -- Snippets source for nvim-cmp
  use 'L3MON4D3/LuaSnip' -- Snippets plugin

  -- git
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }
  use { 'sindrets/diffview.nvim', requires = { 'nvim-lua/plenary.nvim' } }

  -- colorschemes
  -- use 'doums/darcula'
  use 'rebelot/kanagawa.nvim'
end)
