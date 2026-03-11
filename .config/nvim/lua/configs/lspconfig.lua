-- load defaults i.e lua_lsp
require("nvchad.configs.lspconfig").defaults()

-- EXAMPLE
local servers = { "html", "cssls" }

-- Neovim 0.11+: use the new built-in API (nvim-lspconfig provides server defs)
if vim.lsp and vim.lsp.enable and vim.lsp.config then
  vim.lsp.enable(servers)
else
  -- Neovim <= 0.10 fallback
  local lspconfig = require "lspconfig"
  local nvlsp = require "nvchad.configs.lspconfig"

  for _, lsp in ipairs(servers) do
    lspconfig[lsp].setup {
      on_attach = nvlsp.on_attach,
      on_init = nvlsp.on_init,
      capabilities = nvlsp.capabilities,
    }
  end
end

-- configuring single server, example: typescript
-- vim.lsp.config("ts_ls", {
--   -- settings = { ... },
-- })
-- vim.lsp.enable "ts_ls"
