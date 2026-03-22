return {
  dir = vim.fn.expand("~/Development/nvim-beads"),
  name = "nvim-beads",
  config = function()
    require("beads").setup({
      cli_cmd = "br",
      keymaps = false,
      auto_sync = false,
      theme = "dark",
      auto_theme = true,
    })

    local opts = { noremap = true, silent = true }
    vim.keymap.set("n", "<leader>Bd", ":Beads<CR>",              vim.tbl_extend("force", opts, { desc = "Beads: Show task list" }))
    vim.keymap.set("n", "<leader>Bc", ":BeadsCreate ",           vim.tbl_extend("force", opts, { desc = "Beads: Create task" }))
    vim.keymap.set("n", "<leader>Bs", ":BeadsSync<CR>",          vim.tbl_extend("force", opts, { desc = "Beads: Sync" }))
    vim.keymap.set("n", "<leader>Br", ":BeadsRefresh<CR>",       vim.tbl_extend("force", opts, { desc = "Beads: Refresh" }))
    vim.keymap.set("n", "<leader>Bf", ":BeadsFilter ",           vim.tbl_extend("force", opts, { desc = "Beads: Filter" }))
    vim.keymap.set("n", "<leader>BF", ":BeadsClearFilters<CR>",  vim.tbl_extend("force", opts, { desc = "Beads: Clear Filters" }))
    vim.keymap.set("n", "<leader>Bt", ":BeadsFindTask<CR>",      vim.tbl_extend("force", opts, { desc = "Beads: Find Task" }))
    vim.keymap.set("n", "<leader>BS", ":BeadsFindStatus<CR>",    vim.tbl_extend("force", opts, { desc = "Beads: Find Status" }))
    vim.keymap.set("n", "<leader>BP", ":BeadsFindPriority<CR>",  vim.tbl_extend("force", opts, { desc = "Beads: Find Priority" }))
  end,
}
