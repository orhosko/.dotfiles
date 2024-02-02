vim.keymap.set("n", "<leader>nt", ":Neotree toggle<CR>", { desc = "[N]eoTree [T]oggle" })
vim.keymap.set("n", "<leader>nf", ":Neotree position=current<CR>", { desc = "[N]eoTree [F]ullscreen" })

vim.keymap.set("n", "n", "nzzzv", { desc = "n" })
vim.keymap.set("n", "N", "Nzzzv", { desc = "N" })

vim.keymap.set("n", "<C-d>", "<C-d>zz", { desc = "<C-d>" })
vim.keymap.set("n", "<C-u>", "<C-u>zz", { desc = "<C-u>" })

vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '<-2<CR>gv=gv")

vim.keymap.set("x", "<leader>p", [["_dP]])

vim.keymap.set("n", "<leader>o", ":bnext<CR>", { desc = "buffer next" })
vim.keymap.set("n", "<leader>ı", ":bprev<CR>", { desc = "buffer prev" })
vim.keymap.set("n", "<leader>dd", ":bdelete<CR>", { desc = "buffer delete" })

vim.keymap.set({ "i", "v" }, "jk", "<Esc>", { desc = "quick escape" })
vim.keymap.set({ "i", "v" }, "kj", "<Esc>", { desc = "quick escape" })
