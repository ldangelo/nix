return {
  "epwalsh/obsidian.nvim",
  version = "*",
  lazy = true,
  ft = "markdown",
  dependencies = { "nvim-lua/plenary.nvim" },
  keys = {
    { "<leader>on", "<cmd>ObsidianNew<cr>", desc = "New note" },
    { "<leader>oo", "<cmd>ObsidianOpen<cr>", desc = "Open in Obsidian" },
    { "<leader>os", "<cmd>ObsidianSearch<cr>", desc = "Search vault" },
    { "<leader>oq", "<cmd>ObsidianQuickSwitch<cr>", desc = "Quick switch" },
    { "<leader>od", "<cmd>ObsidianDailies<cr>", desc = "Daily notes" },
    { "<leader>ot", "<cmd>ObsidianTags<cr>", desc = "Search tags" },
    { "<leader>ol", "<cmd>ObsidianLinks<cr>", desc = "Show links" },
    { "<leader>ob", "<cmd>ObsidianBacklinks<cr>", desc = "Show backlinks" },
    { "<leader>oc", "<cmd>ObsidianToggleCheckbox<cr>", desc = "Toggle checkbox" },
  },
  opts = function()
    return {
    -- Use the note title as the filename (no timestamp IDs)
    note_id_func = function(title)
      if title ~= nil then
        -- Sanitize title: lowercase, replace spaces with dashes
        return title:gsub(" ", "-"):gsub("[^A-Za-z0-9-]", ""):lower()
      end
      -- Fallback for untitled notes
      return tostring(os.time())
    end,

    -- Use the actual title in wiki links, not the ID
    wiki_link_func = function(opts)
      if opts.label ~= opts.path then
        return string.format("[[%s|%s]]", opts.path, opts.label)
      end
      return string.format("[[%s]]", opts.path)
    end,
    workspaces = {
      { name = "ldangelo", path = "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo" },
      { name = "org", path = "~/Documents/org" },
      { name = "hchb", path = "~/Documents/HCHB" },
    },
    daily_notes = {
      folder = "Daily Notes",
    },
    new_notes_location = "notes_subdir",
    notes_subdir = "Inbox",
    templates = {
      folder = "Templates",
    },
    completion = {
      nvim_cmp = false,
      min_chars = 2,
    },
    ui = {
      checkboxes = {
        [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
        ["x"] = { char = "", hl_group = "ObsidianDone" },
        [">"] = { char = "", hl_group = "ObsidianRightArrow" },
        ["~"] = { char = "󰰱", hl_group = "ObsidianTilde" },
      },
    },
  }
  end,
}
