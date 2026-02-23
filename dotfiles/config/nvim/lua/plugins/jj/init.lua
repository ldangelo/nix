return {
  "nicolasgb/jj.nvim",
  config = function()
    require("jj").setup({
      -- Setup snacks as a picker
      picker = {
        -- Here you can pass the options as you would for snacks.
        -- It will be used when using the picker
        snacks = {},
      },

      -- Choose the editor mode for describe command
      -- "buffer" - Opens a Git-style commit message buffer with syntax highlighting (default)
      -- "input" - Uses a simple vim.ui.input prompt
      describe_editor = "buffer",

      -- Customize syntax highlighting colors for the describe buffer
      highlights = {
        added = { fg = "#3fb950", ctermfg = "Green" },    -- Added files
        modified = { fg = "#56d4dd", ctermfg = "Cyan" },  -- Modified files
        deleted = { fg = "#f85149", ctermfg = "Red" },    -- Deleted files
        renamed = { fg = "#d29922", ctermfg = "Yellow" }, -- Renamed files
      },
    })
  end,
  keys = {
    { "<leader>jl", function() require("jj.log").log() end, desc = "Log" },
    { "<leader>js", function() require("jj.status").status() end, desc = "Status" },
    { "<leader>jd", function() require("jj.diff").diff() end, desc = "Diff" },
    { "<leader>jD", function() require("jj.diff").diff_current() end, desc = "Diff current file" },
    { "<leader>jn", function() require("jj.commands").new() end, desc = "New change" },
    { "<leader>je", function() require("jj.commands").edit() end, desc = "Edit change" },
    { "<leader>jc", function() require("jj.describe").commit() end, desc = "Describe/commit" },
    { "<leader>jq", function() require("jj.commands").squash() end, desc = "Squash" },
    { "<leader>jr", function() require("jj.commands").rebase() end, desc = "Rebase" },
    { "<leader>jp", function() require("jj.push").push() end, desc = "Push" },
    { "<leader>jf", function() require("jj.commands").fetch() end, desc = "Fetch" },
    { "<leader>jb", function() require("jj.bookmark").bookmark_create() end, desc = "Create bookmark" },
    { "<leader>jB", function() require("jj.browse").browse() end, desc = "Browse (open PR)" },
    { "<leader>jh", function() require("jj.file").file_history() end, desc = "File history" },
    { "<leader>ja", function() require("jj.commands").abandon() end, desc = "Abandon change" },
    { "<leader>ju", function() require("jj.commands").undo() end, desc = "Undo" },
    { "<leader>jR", function() require("jj.commands").redo() end, desc = "Redo" },
  },
}
