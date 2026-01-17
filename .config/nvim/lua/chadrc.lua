local M = {}

M.base46 = {
  theme = "wombat",

  hl_override = {
    Comment = { fg = "#afaf52" },
    ["@comment"] = { fg = "#afaf52" },
  },
}

M.ui = {
  statusline = {
    theme = "default",
    separator_style = "round",
    order = {
      "mode",
      "file",
      "git",
      "line_col_pos",
      "%=",
      "lsp_msg",
      "%=",
      "lsp",
      "cwd",
    },
    modules = {
      line_col_pos = function()
        local line = vim.api.nvim_win_get_cursor(0)[1]
        local col = vim.api.nvim_win_get_cursor(0)[2]
        local text = string.format(" %02d:%02d ", line, col)
        return "%#Title#" .. text
      end,
    },
  },
}

return M
