return {
  dir = vim.fn.expand("~/Development/nvim-beads"),
  name = "nvim-beads",
  config = function()
    require("beads").setup({
      cli_cmd = "br",
      keymaps = true,
      auto_sync = false,
      theme = "dark",
      auto_theme = true,
    })
  end,
}
