vim.g.mapleader = " "
vim.keymap.set("n", "<leader>pv", [[<cmd>:NvimTreeToggle <cr>]])
vim.keymap.set("n", "<leader>tn", [[<cmd>:tabnew <cr>]])
vim.keymap.set("n", "<leader>tc", [[<cmd>:tabc <cr>]])
vim.keymap.set("n", "<C-i>", [[<cmd>:lua vim.lsp.buf.format() <cr>]])

-- Move Lines or Block up or Down
vim.keymap.set("v", "<A-j>", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "<A-k>", ":m '<-2<CR>gv=gv")
vim.keymap.set("n", "<M-k>", [[<cmd>:move .-2 <cr>]])
vim.keymap.set("n", "<M-j>", [[<cmd>:move .+1 <cr>]])
