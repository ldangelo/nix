-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
-- Add any additional autocmds here

-- Set keymap for markdown files
vim.api.nvim_create_autocmd("FileType", {
  pattern = "md",
  callback = function()
    vim.keymap.set("n", "<localleader>p", ":MarkdownPreview<CR>", { desc = "Preview Markdown", buffer = true })
  end,
})
