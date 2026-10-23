local wezterm = require("wezterm")
local ai_helper = wezterm.plugin.require("https://github.com/ldangelo/ai-helper.wezterm")

-- configure to use openrouter and sonnet
local config = wezterm.config_builder()
ai_helper.apply_to_config(config, {
	type = "http",
	api_url = "https://api.openrouter.ai/v1/chat/completions", -- or your service URL
	api_key = "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", -- if required
	model = "openrouter/auto", -- model name
})
