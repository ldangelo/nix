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
                -- Multiplexer integration (auto-detects Zellij, Tmux, Wezterm, Kitty)
                multiplexer_integration = nil, -- nil = auto-detect, 'zellij' to force Zellij
                -- Disable multiplexer when in certain terminals if needed
                -- multiplexer_integration = 'zellij',
            })
        --
        -- Keymaps are configured in lua/config/keymaps.lua:
        -- Ctrl+hjkl for navigation (seamless with Zellij via vim-zellij-navigator)
        -- Cmd+hjkl for resizing (matches WezTerm, avoids AeroSpace conflict)
    end,
}
