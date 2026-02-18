return {
  "mikavilpas/yazi.nvim",
  event = "VeryLazy",
  keys = {
    { "<leader>fy", "<cmd>Yazi<cr>", desc = "Open yazi (current file)" },
    { "<leader>fY", "<cmd>Yazi cwd<cr>", desc = "Open yazi (cwd)" },
  },
  opts = {
    open_for_directories = false,
  },
}
