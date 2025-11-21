return {
  "catppuccin/nvim",
  name = "catppuccin",
  priority = 1000,
  enabled = true,
  config = function()
    catppuccin = require("catppuccine").setup()({
      auto_integrations = true,
    })
  end,
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "catppuccin",
    },
  },
}
