return {
  "coder/claudecode.nvim",
  dependencies = { "folke/snacks.nvim" },
  config = function()
    require("claudecode").setup()

    -- Autocmd to unmap C-j and C-k in Claude Code terminal buffers
    vim.api.nvim_create_autocmd("TermOpen", {
      pattern = "*",
      callback = function()
        vim.defer_fn(function()
          -- Check if this is a Claude Code terminal
          local bufname = vim.api.nvim_buf_get_name(0)
          if bufname:match("claudecode") or bufname:match("claude") then
            -- Unmap any existing C-j and C-k mappings in terminal mode
            pcall(vim.keymap.del, "t", "<C-j>", { buffer = true })
            pcall(vim.keymap.del, "t", "<C-k>", { buffer = true })

            -- Set smart-splits keymaps for this terminal buffer
            vim.keymap.set("t", "<C-j>", require("smart-splits").move_cursor_down, { buffer = true, silent = true })
            vim.keymap.set("t", "<C-k>", require("smart-splits").move_cursor_up, { buffer = true, silent = true })
            vim.keymap.set("t", "<C-h>", require("smart-splits").move_cursor_left, { buffer = true, silent = true })
            vim.keymap.set("t", "<C-l>", require("smart-splits").move_cursor_right, { buffer = true, silent = true })
          end
        end, 100)
      end,
    })
  end,
  keys = {
    { "<leader>a",  nil,                              desc = "AI/Claude Code" },
    { "<leader>ac", "<cmd>ClaudeCode<cr>",            desc = "Toggle Claude" },
    { "<leader>af", "<cmd>ClaudeCodeFocus<cr>",       desc = "Focus Claude" },
    { "<leader>ar", "<cmd>ClaudeCode --resume<cr>",   desc = "Resume Claude" },
    { "<leader>aC", "<cmd>ClaudeCode --continue<cr>", desc = "Continue Claude" },
    { "<leader>am", "<cmd>ClaudeCodeSelectModel<cr>", desc = "Select Claude model" },
    { "<leader>ab", "<cmd>ClaudeCodeAdd %<cr>",       desc = "Add current buffer" },
    { "<leader>as", "<cmd>ClaudeCodeSend<cr>",        mode = "v",                  desc = "Send to Claude" },
    {
      "<leader>as",
      "<cmd>ClaudeCodeTreeAdd<cr>",
      desc = "Add file",
      ft = { "NvimTree", "neo-tree", "oil", "minifiles", "netrw" },
    },
    -- Diff management
    { "<leader>aa", "<cmd>ClaudeCodeDiffAccept<cr>", desc = "Accept diff" },
    { "<leader>ad", "<cmd>ClaudeCodeDiffDeny<cr>",   desc = "Deny diff" },
  },
}
