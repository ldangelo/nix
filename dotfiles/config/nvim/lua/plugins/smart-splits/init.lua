return {
    "mrjones2014/smart-splits.nvim",

    config = function()
        enabled =
            function()
                if not vim.g.vscode == nil then
                    return true
                end
                return false
            end, require("smart-splits").setup({
                -- Multiplexer integration (auto-detects Tmux, Wezterm, Kitty)
                multiplexer_integration = nil, -- nil = auto-detect, 'tmux' to force tmux
            })
        --
        -- Keymaps are configured in lua/config/keymaps.lua:
        -- Ctrl+hjkl for navigation (seamless with tmux via vim-tmux-navigator)
        -- Cmd+hjkl for resizing (matches WezTerm, avoids AeroSpace conflict)
    end,
}
