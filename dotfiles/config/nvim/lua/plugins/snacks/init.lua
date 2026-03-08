return {
  "folke/snacks.nvim",
  opts = {
    dim = { enabled = true },
    zen = { enabled = true },
    scroll = { enabled = true },
    indent = { enabled = true },
    image = { enabled = true },
    scope = { enabled = true },
    explorer = { enabled = true },
    words = { enabled = true },
    picker = {
      sources = {
        files = {
          hidden = true, -- show hidden files (e.g. .gitignore, .env)
        },
      },
    },
  },
  keys = {
    { "<leader>z", function() Snacks.zen() end, desc = "Zen Mode" },
    { "<leader>Z", function() Snacks.zen.zoom() end, desc = "Zen Zoom" },
    { "<leader>gB", function() Snacks.gitbrowse() end, desc = "Git Browse", mode = { "n", "v" } },
    { "]]", function() Snacks.words.jump(vim.v.count1) end, desc = "Next Reference", mode = { "n", "t" } },
    { "[[", function() Snacks.words.jump(-vim.v.count1) end, desc = "Prev Reference", mode = { "n", "t" } },
  },
}
