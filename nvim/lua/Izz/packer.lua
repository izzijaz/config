-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'
    -- install tree view
    use 'nvim-tree/nvim-tree.lua'
    use 'nvim-tree/nvim-web-devicons'
    --
    -- Better Diagnostics 
    use "folke/trouble.nvim"
    use 'folke/lsp-colors.nvim'
    use "Fildo7525/pretty_hover"
    use 'Civitasv/cmake-tools.nvim'
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.1',
        -- or                            , branch = '0.1.x',
        requires = { { 'nvim-lua/plenary.nvim' } }
    }
    use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
    use {
        "windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }
    use { "catppuccin/nvim", as = "catppuccin" }
    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'nvim-tree/nvim-web-devicons', opt = true }
    }
    use {
        'VonHeikemen/lsp-zero.nvim',
        branch = 'v2.x',
        requires = {
            -- LSP Support
            { 'neovim/nvim-lspconfig' },             -- Required
            { 'williamboman/mason.nvim' },
            { 'williamboman/mason-lspconfig.nvim' }, -- Optional

            -- Autocompletion
            { 'hrsh7th/nvim-cmp' },     -- Required
            { 'hrsh7th/cmp-nvim-lsp' }, -- Required
            { 'L3MON4D3/LuaSnip' },     -- Required
            { "hrsh7th/cmp-path" },
            { 'saadparwaiz1/cmp_luasnip' },
            { "hrsh7th/cmp-nvim-lua" },
            { "ray-x/lsp_signature.nvim" },
            { "rafamadriz/friendly-snippets" },
            { "onsails/lspkind.nvim" },
            { "hrsh7th/cmp-cmdline" }
        }
    }
    use('ThePrimeagen/vim-be-good')
    use "brenoprata10/nvim-highlight-colors"
    use { 'mhartington/formatter.nvim' }
    use { "jose-elias-alvarez/null-ls.nvim" }
    use { "xiyaowong/transparent.nvim" }
    use 'mrcjkb/haskell-tools.nvim'
end)
