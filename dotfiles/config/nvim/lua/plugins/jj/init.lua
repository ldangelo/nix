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
    -- Use the exposed functions directly from the main module
  end,
}
