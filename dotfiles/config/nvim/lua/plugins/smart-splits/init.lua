return {
  "mrjones2014/smart-splits.nvim",

  config = function()
    enabled =
      function()
        if not vim.g.vscode == nil then
          return true
        end
        return false
      end, require("smart-splits").setup({})
    --
    -- recommended mappings
    -- resizing splits
    -- these keymaps will also accept a range,
    -- for example `10<A-h>` will `resize_left` by `(10 * config.default_amount)`
  end,
}
