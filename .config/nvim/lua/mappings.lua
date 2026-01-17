require "nvchad.mappings"

local map = vim.keymap.set

map("n", ";", ":", { desc = "CMD enter command mode" })

map('n', '<M-n>', ':bn<cr>', { noremap = true})
map('n', '<M-p>', ':bp<cr>', { noremap = true})

map('c', '<M-BS>', '<C-w>', { noremap = true, desc =
  "Delete word backward"})
map('c', '<C-a>', '<Home>', { noremap = true })
map('c', '<C-e>', '<End>', { noremap = true })
map('c', '<C-k>', '<C-End>', { noremap = true })
map('c', '<M-b>', '<S-Left>', { noremap = true })
map('c', '<M-f>', '<S-Right>', { noremap = true })

map('i', '<M-BS>', '<C-w>', { noremap = true, desc =
  "Delete word backward"})
map('i', '<M-d>', '<C-o>de', { noremap = true, desc =
  "Delete word forward" })
map('i', '<C-a>', '<Home>', { noremap = true })
map('i', '<C-e>', '<End>', { noremap = true })
map('i', '<C-k>', '<C-o>d$', { noremap = true })
map('i', '<M-b>', '<S-Left>', { noremap = true })
map('i', '<M-f>', '<S-Right>', { noremap = true })

map('i', '<M-n>', '<Esc> :bn<cr> i', { noremap = true})
map('i', '<M-p>', '<Esc> :bp<cr> i', { noremap = true})

map('i', '<Tab>', '<C-o>==<C-o>^',
  { noremap = true, silent = true, expr = false, desc = "Smart Indent Line on Tab Press" })

-- Reflow current paragraph (comment-aware)
--vim.keymap.set('i', '<M-q>', '<C-o>gqip', { desc = 'Reflow paragraph (comment aware)' })

-- Alt-q: reflow the current comment paragraph, keep cursor position
local reflow = function()
  vim.cmd("normal! mz")
  vim.cmd("normal! gqip")
  vim.cmd("normal! `z")
end
map('i', '<M-q>', reflow, { desc = "reflow comment paragraph" })
map('n', '<M-q>', reflow, { desc = "reflow comment paragraph" })

-- Visual mode: reflow the selected region
vim.keymap.set('v', '<M-q>', 'gq', { desc = 'Reflow selection' })
