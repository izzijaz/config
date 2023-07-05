require("Izz.remap")
require("Izz.set")
require("Izz.packer")
require("Izz.lualine")
require("catppuccin").setup({
  transparent_background = false,
  term_colors = true,
  color_overrides = {
      all = {text="#ffffff"}
  }
})
vim.cmd [[colorscheme catppuccin]]
vim.cmd [[hi LineNr guifg=#aaaaaa]]
