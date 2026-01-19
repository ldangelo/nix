-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Leader keys (explicitly documented for clarity)
vim.g.mapleader = " "      -- Space (LazyVim default, but explicit for documentation)
vim.g.maplocalleader = "," -- Comma for buffer-local mappings

-- Python LSP configuration
vim.g.lazyvim_python_lsp = "basedpyright"
