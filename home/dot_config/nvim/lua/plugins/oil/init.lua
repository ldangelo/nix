return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
  keys = {
    { "-", "<CMD>Oil<CR>", desc = "Open parent directory" },
    { "<leader>fe", "<CMD>Oil<CR>", desc = "Oil file explorer" },
    { "<leader>fE", "<CMD>Oil --float<CR>", desc = "Oil float" },
  },
  ---@module 'oil'
  ---@type oil.SetupOpts
  opts = {
    default_file_explorer = true,
    delete_to_trash = true,
    skip_confirm_for_simple_edits = false,
    view_options = {
      show_hidden = true,
      natural_order = "fast",
    },
    columns = { "icon" },
    lsp_file_method = {
      timeout_ms = 1000,
      autosave_changes = true,
    },
    keymaps = {
      ["g?"] = "actions.show_help",
      ["<CR>"] = "actions.select",
      ["<C-s>"] = "actions.select_vsplit",
      ["<C-v>"] = "actions.select_split",  -- moved from <C-h> to avoid pane nav conflict
      ["<C-t>"] = "actions.select_tab",
      ["-"] = "actions.parent",
      ["_"] = "actions.open_cwd",
      ["`"] = "actions.cd",
      ["~"] = "actions.tcd",
      ["gs"] = "actions.change_sort",
      ["gx"] = "actions.open_external",
      ["g."] = "actions.toggle_hidden",
      ["g\\"] = "actions.toggle_trash",
      ["<C-p>"] = "actions.preview",
      ["<C-r>"] = "actions.refresh",       -- moved from <C-l> to avoid pane nav conflict
      -- Pane navigation (smart-splits compatible)
      ["<C-h>"] = function() require("smart-splits").move_cursor_left() end,
      ["<C-j>"] = function() require("smart-splits").move_cursor_down() end,
      ["<C-k>"] = function() require("smart-splits").move_cursor_up() end,
      ["<C-l>"] = function() require("smart-splits").move_cursor_right() end,
    },
  },
}
