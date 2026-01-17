require "nvchad.options"

local o = vim.o

-- Change the cwd to the current buffer's directory.
o.autochdir = true

o.expandtab = true
o.tabstop = 2
o.shiftwidth = 2
o.softtabstop = 2
o.autoindent = true
o.smartindent = true

-- Enable list mode to see whitespace characters.
o.list = true

-- Adjust how gq formats text.
--
-- https://neovim.io/doc/user/change.html#fo-table
--
-- t: auto-wrap text using 'textwidth'
-- c: auto-wrap comments using 'textwidth'
-- q: allow formatting of comments with gq
-- j: remove comment leader when joining lines
-- n: recognize numbered lists
-- r: continue comment when pressing <CR>
vim.opt.formatoptions:append('tcqjnr')

