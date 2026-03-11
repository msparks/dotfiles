vim.g.base46_cache = vim.fn.stdpath "data" .. "/base46/"
vim.g.mapleader = " "

-- bootstrap lazy and all plugins
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"

if not vim.uv.fs_stat(lazypath) then
  local repo = "https://github.com/folke/lazy.nvim.git"
  vim.fn.system { "git", "clone", "--filter=blob:none", repo, "--branch=stable", lazypath }
end

vim.opt.rtp:prepend(lazypath)

local lazy_config = require "configs.lazy"

-- load plugins
require("lazy").setup({
  {
    "NvChad/NvChad",
    lazy = false,
    branch = "v2.5",
    import = "nvchad.plugins",
  },

  { import = "plugins" },
}, lazy_config)

-- load theme
dofile(vim.g.base46_cache .. "defaults")
dofile(vim.g.base46_cache .. "statusline")

require "options"
require "nvchad.autocmds"

vim.schedule(function()
  require "mappings"
end)

vim.api.nvim_create_autocmd("FileType", {
  pattern = "gitcommit",
  callback = function()
    vim.cmd("startinsert")
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "cue",
  callback = function()
    -- CUE (like Go) prefers hard tabs for indentation.
    vim.opt_local.expandtab = false
    -- Render tabs as 2 columns (personal preference).
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.softtabstop = 0
  end,
})

vim.api.nvim_create_user_command("Format", function()
  local ok, conform = pcall(require, "conform")
  if not ok then
    local ok_lazy, lazy = pcall(require, "lazy")
    if ok_lazy then
      lazy.load { plugins = { "conform.nvim" } }
      ok, conform = pcall(require, "conform")
    end
  end

  if not ok then
    vim.notify("Format: conform.nvim not available", vim.log.levels.ERROR)
    return
  end

  conform.format { lsp_fallback = true }
end, {})
