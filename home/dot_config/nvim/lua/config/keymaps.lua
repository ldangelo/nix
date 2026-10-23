-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local keymap = vim.keymap.set
keymap("n", "<leader>fs", ":w<CR>")
keymap("n", "<leader>fS", ":wall<CR>")

if not vim.g.vscode then
    -- Resizing splits - Changed from Alt to Cmd to avoid conflict with AeroSpace
    -- Cmd+hjkl matches WezTerm's Cmd+hjkl for pane resizing (consistency!)
    vim.keymap.set("n", "<D-h>", require("smart-splits").resize_left)
    vim.keymap.set("n", "<D-j>", require("smart-splits").resize_down)
    vim.keymap.set("n", "<D-k>", require("smart-splits").resize_up)
    vim.keymap.set("n", "<D-l>", require("smart-splits").resize_right)

    -- Moving between splits - Ctrl+hjkl for seamless navigation
    -- Works across Zellij panes AND Neovim splits via vim-zellij-navigator
    vim.keymap.set("n", "<C-h>", require("smart-splits").move_cursor_left)
    vim.keymap.set("n", "<C-j>", require("smart-splits").move_cursor_down)
    vim.keymap.set("n", "<C-k>", require("smart-splits").move_cursor_up)
    vim.keymap.set("n", "<C-l>", require("smart-splits").move_cursor_right)
    vim.keymap.set("n", "<C-\\>", require("smart-splits").move_cursor_previous)

    -- Swapping buffers between windows
    vim.keymap.set("n", "<leader><leader>h", require("smart-splits").swap_buf_left)
    vim.keymap.set("n", "<leader><leader>j", require("smart-splits").swap_buf_down)
    vim.keymap.set("n", "<leader><leader>k", require("smart-splits").swap_buf_up)
    vim.keymap.set("n", "<leader><leader>l", require("smart-splits").swap_buf_right)
end
