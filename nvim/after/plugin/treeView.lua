require("nvim-tree").setup({
  sort_by = "case_sensitive",
  view = {
    width = 30,
    preserve_window_proportions = true,
  },
  renderer = {
    group_empty = true,
  },
  filters = {
    dotfiles = true,
  },
})
