return {
  {
    "j-hui/fidget.nvim",
    opts = {
      notification = {
        window = {
          border = "rounded",
          winblend = 10,
        },
        view = {
          icon = {
            info = " ",
            warn = " ",
            error = " ",
            debug = " ",
          },
        },
      },
      progress = {
        poll_rate = 100,
        display = {
          done_icon = "✔️ ",
          running_icon = "⏳ ",
        },
      },
    },
  },
}
