vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)
vim.keymap.set("n", "<C-i>", [[<cmd>:lua vim.lsp.buf.format() <cr>]])
