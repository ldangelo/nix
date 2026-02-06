local wezterm = require("wezterm")
local utils = require("utils")
local keybinds = require("keybinds")
local gpus = wezterm.gui.enumerate_gpus()
require("on")

---------------------------------------------------------------
--- Plugins
---------------------------------------------------------------
local resurrect = wezterm.plugin.require("https://github.com/MLFlexer/resurrect.wezterm")
local workspace_switcher = wezterm.plugin.require("https://github.com/MLFlexer/smart_workspace_switcher.wezterm")
local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")

-- /etc/ssh/sshd_config
-- AcceptEnv TERM_PROGRAM_VERSION COLORTERM TERM TERM_PROGRAM WEZTERM_REMOTE_PANE
-- sudo systemctl reload sshd

---------------------------------------------------------------
--- functions
---------------------------------------------------------------
-- selene: allow(unused_variable)
---@diagnostic disable-next-line: unused-function, unused-local
local function enable_wayland()
    local wayland = os.getenv("XDG_SESSION_TYPE")
    if wayland == "wayland" then
        return true
    end
    return false
end

---------------------------------------------------------------
--- Merge the Config
---------------------------------------------------------------
local function create_ssh_domain_from_ssh_config(ssh_domains)
    if ssh_domains == nil then
        ssh_domains = {}
    end
    for host, config in pairs(wezterm.enumerate_ssh_hosts()) do
        table.insert(ssh_domains, {
            name = host,
            remote_address = config.hostname .. ":" .. config.port,
            username = config.user,
            multiplexing = "None",
            assume_shell = "Posix",
        })
    end
    return { ssh_domains = ssh_domains }
end

--- load local_config
-- Write settings you don't want to make public, such as ssh_domains
package.path = os.getenv("HOME") .. "/.local/share/wezterm/?.lua;" .. package.path
local function load_local_config(module)
    local m = package.searchpath(module, package.path)
    if m == nil then
        return {}
    end
    return dofile(m)
    -- local ok, _ = pcall(require, "local")
    -- if not ok then
    -- 	return {}
    -- end
    -- return require("local")
end

local local_config = load_local_config("local")

-- local local_config = {
-- 	ssh_domains = {
-- 		{
-- 			-- This name identifies the domain
-- 			name = "my.server",
-- 			-- The address to connect to
-- 			remote_address = "192.168.8.31",
-- 			-- The username to use on the remote host
-- 			username = "katayama",
-- 		},
-- 	},
-- }
-- return local_config

---------------------------------------------------------------
--- Config
---------------------------------------------------------------
local config = {
    -- font = wezterm.font("Cica"),
    -- font_size = 10.0,
    font = wezterm.font("Source Code Pro"),
    font_size = 14,
    -- cell_width = 1.1,
    -- line_height = 1.1,
    -- font_rules = 14{
    -- 	{
    -- 		italic = true,
    -- 		font = wezterm.font("Cica", { italic = true }),
    -- 	},
    -- 	{
    -- 		italic = true,
    -- 		intensity = "Bold",
    -- 		font = wezterm.font("Cica", { weight = "Bold", italic = true }),
    -- 	},
    -- },
    check_for_updates = false,
    use_ime = true,
    ime_preedit_rendering = "Builtin",
    use_dead_keys = false,
    warn_about_missing_glyphs = false,
    -- enable_kitty_graphics = false,
    animation_fps = 1,
    cursor_blink_ease_in = "Constant",
    cursor_blink_ease_out = "Constant",
    cursor_blink_rate = 0,
    -- https://github.com/wez/wezterm/issues/4972
    -- enable_wayland = enable_wayland(),
    enable_wayland = false,
    -- https://github.com/wez/wezterm/issues/1772
    -- https://github.com/wez/wezterm/issues/5103
    -- enable_wayland = false,
    -- Catppuccin Mocha theme (matches qutebrowser)
    color_scheme = "Catppuccin Mocha",

    -- Minimal UI (matches qutebrowser's tabs.show = "multiple" and statusbar.show = "in-mode")
    hide_tab_bar_if_only_one_tab = true,
    use_fancy_tab_bar = false,
    tab_bar_at_bottom = true,
    tab_max_width = 32,
    show_tab_index_in_tab_bar = false,
    show_new_tab_button_in_tab_bar = false,

    -- Window styling
    window_decorations = "RESIZE",
    adjust_window_size_when_changing_font_size = false,
    window_background_opacity = 1.0,
    macos_window_background_blur = 0,

    -- Subtle padding (clean look)
    window_padding = {
        left = 8,
        right = 8,
        top = 8,
        bottom = 8,
    },

    -- Dim inactive panes to highlight the active one
    inactive_pane_hsb = {
        saturation = 0.7,
        brightness = 0.6,
    },

    selection_word_boundary = " \t\n{}[]()\"'`,;:│=&!%",
    exit_behavior = "CloseOnCleanExit",
    window_close_confirmation = "NeverPrompt",
    -- window_background_opacity = 0.8,
    disable_default_key_bindings = true,
    -- visual_bell = {
    -- 	fade_in_function = "EaseIn",
    -- 	fade_in_duration_ms = 150,
    -- 	fade_out_function = "EaseOut",
    -- 	fade_out_duration_ms = 150,
    -- },
    -- separate <Tab> <C-i>
    enable_csi_u_key_encoding = true,
    -- Leader key: Cmd+Space (ergonomic, doesn't conflict with Spotlight in terminal)
    leader = { key = "Space", mods = "CMD" },
    keys = keybinds.create_keybinds(),
    key_tables = keybinds.key_tables,
    mouse_bindings = keybinds.mouse_bindings,
    -- https://github.com/wez/wezterm/issues/2756
    webgpu_preferred_adapter = gpus[1],
    front_end = "OpenGL",
}

-- https://github.com/wez/wezterm/commit/1e552d764349522dabffeb240feb5b2728eff3d8
-- for _, gpu in ipairs(wezterm.gui.enumerate_gpus()) do
-- 	if gpu.backend == "Vulkan" and gpu.device_type == "IntegratedGpu" then
-- 		config.webgpu_preferred_adapter = gpu
-- 		config.front_end = "WebGpu"
-- 		break
-- 	end
-- end

config.hyperlink_rules = {
    -- Matches: a URL in parens: (URL)
    {
        regex = "\\((\\w+://\\S+)\\)",
        format = "$1",
        highlight = 1,
    },
    -- Matches: a URL in brackets: [URL]
    {
        regex = "\\[(\\w+://\\S+)\\]",
        format = "$1",
        highlight = 1,
    },
    -- Matches: a URL in curly braces: {URL}
    {
        regex = "\\{(\\w+://\\S+)\\}",
        format = "$1",
        highlight = 1,
    },
    -- Matches: a URL in angle brackets: <URL>
    {
        regex = "<(\\w+://\\S+)>",
        format = "$1",
        highlight = 1,
    },
    -- Then handle URLs not wrapped in brackets
    {
        -- Before
        --regex = '\\b\\w+://\\S+[)/a-zA-Z0-9-]+',
        --format = '$0',
        -- After
        regex = "[^(]\\b(\\w+://\\S+[)/a-zA-Z0-9-]+)",
        format = "$1",
        highlight = 1,
    },
    -- implicit mailto link
    {
        regex = "\\b\\w+@[\\w-]+(\\.[\\w-]+)+\\b",
        format = "mailto:$0",
    },
}
table.insert(config.hyperlink_rules, {
    regex = [[["]?([\w\d]{1}[-\w\d]+)(/){1}([-\w\d\.]+)["]?]],
    format = "https://github.com/$1/$3",
})

local w = require("wezterm")

-- if you are *NOT* lazy-loading smart-splits.nvim (recommended)
local function is_vim(pane)
    -- this is set by the plugin, and unset on ExitPre in Neovim
    return pane:get_user_vars().IS_NVIM == "true"
end

local direction_keys = {
    h = "Left",
    j = "Down",
    k = "Up",
    l = "Right",
}

local function split_nav(resize_or_move, key)
    return {
        key = key,
        mods = resize_or_move == "resize" and "META" or "CTRL",
        action = w.action_callback(function(win, pane)
            if is_vim(pane) then
                -- pass the keys through to vim/nvim
                win:perform_action({
                    SendKey = { key = key, mods = resize_or_move == "resize" and "META" or "CTRL" },
                }, pane)
            else
                if resize_or_move == "resize" then
                    win:perform_action({ AdjustPaneSize = { direction_keys[key], 3 } }, pane)
                else
                    win:perform_action({ ActivatePaneDirection = direction_keys[key] }, pane)
                end
            end
        end),
    }
end

config.keys = utils.merge_lists(config.keys, {
    split_nav("resize", "h"),
    split_nav("resize", "j"),
    split_nav("resize", "k"),
    split_nav("resize", "l"),
    split_nav("move", "h"),
    split_nav("move", "j"),
    split_nav("move", "k"),
    split_nav("move", "l"),
})

--require("plugins/ai_helper")

---------------------------------------------------------------
--- Plugin Setup
---------------------------------------------------------------

-- Tabline setup (Catppuccin theme, minimal style)
tabline.setup({
    options = {
        theme = "Catppuccin Mocha",
        section_separators = { left = "", right = "" },
        component_separators = { left = "", right = "" },
        tab_separators = { left = "", right = "" },
    },
    sections = {
        tabline_a = { "workspace" },
        tabline_b = {},
        tabline_c = {},
        tab_active = { "index", { "cwd", padding = 1 } },
        tab_inactive = { "index", { "process", padding = 1 } },
        tabline_x = {},
        tabline_y = { "datetime" },
        tabline_z = { "hostname" },
    },
})
tabline.apply_to_config(config)

-- Resurrect plugin setup (session management)
-- Note: periodic_save is not available in current version of resurrect plugin
-- resurrect.periodic_save({
-- 	interval_seconds = 300, -- Save every 5 minutes
-- 	save_tabs = true,
-- 	save_windows = true,
-- 	save_workspaces = true,
-- })

-- Workspace switcher setup (uses zoxide for smart directory picking)
workspace_switcher.zoxide_path = "/etc/profiles/per-user/ldangelo/bin/zoxide"

-- Plugin keybindings (using LEADER key: Cmd+Space)
config.keys = utils.merge_lists(config.keys, {
    -- Resurrect: Save state
    {
        key = "S",
        mods = "LEADER",
        action = wezterm.action_callback(function(win, pane)
            resurrect.save_state(resurrect.workspace_state.get_workspace_state())
            win:toast_notification("WezTerm", "Session saved!", nil, 2000)
        end),
    },
    -- Resurrect: Load state
    {
        key = "R",
        mods = "LEADER",
        action = wezterm.action_callback(function(win, pane)
            resurrect.fuzzy_load(win, pane, function(id, label)
                local state = resurrect.load_state(id, "workspace")
                resurrect.workspace_state.restore_workspace(state, {
                    relative = true,
                    restore_text = true,
                    on_pane_restore = resurrect.tab_state.default_on_pane_restore,
                })
            end, {
                title = "Load Session",
                fuzzy_description = "Search sessions: ",
            })
        end),
    },
    -- Resurrect: Delete state
    {
        key = "D",
        mods = "LEADER",
        action = wezterm.action_callback(function(win, pane)
            resurrect.fuzzy_load(win, pane, function(id)
                resurrect.delete_state(id)
                win:toast_notification("WezTerm", "Session deleted!", nil, 2000)
            end, {
                title = "Delete Session",
                fuzzy_description = "Search sessions to delete: ",
                is_fuzzy = true,
            })
        end),
    },
    -- Workspace switcher
    {
        key = "w",
        mods = "LEADER",
        action = workspace_switcher.switch_workspace(),
    },
    -- Workspace switcher: Create new workspace
    {
        key = "W",
        mods = "LEADER",
        action = wezterm.action.PromptInputLine({
            description = "Enter new workspace name:",
            action = wezterm.action_callback(function(window, pane, line)
                if line then
                    window:perform_action(
                        wezterm.action.SwitchToWorkspace({ name = line }),
                        pane
                    )
                end
            end),
        }),
    },
})

local merged_config = utils.merge_tables(config, local_config)
return utils.merge_tables(merged_config, create_ssh_domain_from_ssh_config(merged_config.ssh_domains))
