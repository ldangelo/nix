-- workspaces.lua — workspace layouts moved to tmuxinator
-- WezTerm is now a dumb terminal; tmux handles all multiplexing.
local M = {}
function M.augment_command_palette() return {} end
function M.workspace_launcher_action() return require("wezterm").action.Nop end
return M
