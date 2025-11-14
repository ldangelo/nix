require("config.util")
require("config.options")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = {
    -- add LazyVim and import its plugins
    { "LazyVim/LazyVim", import = "lazyvim.plugins" },
    --    { import = "lazyvim.plugins.extras.lang.omnisharp" },
    -- import/override with your plugins
    { import = "plugins" },
  },
  defaults = {
    -- By default, only LazyVim plugins will be lazy-loaded. Your custom plugins will load during startup.
    -- If you know what you're doing, you can set this to `true` to have all your custom plugins lazy-loaded by default.
    lazy = false,
    -- It's recommended to leave version=false for now, since a lot the plugin that support versioning,
    -- have outdated releases, which may break your Neovim install.
    version = false, -- always use the latest git commit
    -- version = "*", -- try installing the latest stable version for plugins that support semver
  },
  --  install = { colorscheme = { "tokyonight", "habamax" } },
  checker = {
    enabled = true, -- check for plugin updates periodically
    notify = false, -- notify on update
  }, -- automatically check for plugin updates
  performance = {
    rtp = {
      -- disable some rtp plugins
      disabled_plugins = {
        "gzip",
        -- "matchit",
        -- "matchparen",
        -- "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})

-- if not vim.g.vscode then
--   require("lspconfig").omnisharp.setup({
--     cmd = { "dotnet", "/Users/ldangelo/.local/share/nvim/mason/packages/omnisharp/libexec/OmniSharp.dll" },
--
--     settings = {
--       FormattingOptions = {
--         -- Enables support for reading code style, naming convention and analyzer
--         -- settings from .editorconfig.
--         EnableEditorConfigSupport = true,
--         -- Specifies whether 'using' directives should be grouped and sorted during
--         -- document formatting.
--         OrganizeImports = true,
--       },
--       MsBuild = {
--         -- If true, MSBuild project system will only load projects for files that
--         -- were opened in the editor. This setting is useful for big C# codebases
--         -- and allows for faster initialization of code navigation features only
--         -- for projects that are relevant to code that is being edited. With this
--         -- setting enabled OmniSharp may load fewer projects and may thus display
--         -- incomplete reference lists for symbols.
--         LoadProjectsOnDemand = nil,
--       },
--       RoslynExtensionsOptions = {
--         -- Enables support for roslyn analyzers, code fixes and rulesets.
--         EnableAnalyzersSupport = false,
--         -- Enables support for showing unimported types and unimported extension
--         -- methods in completion lists. When committed, the appropriate using
--         -- directive will be added at the top of the current file. This option can
--         -- have a negative impact on initial completion responsiveness,
--         -- particularly for the first few completion sessions after opening a
--         -- solution.
--         EnableImportCompletion = true,
--         -- Only run analyzers against open files when 'enableRoslynAnalyzers' is
--         -- true
--         AnalyzeOpenDocumentsOnly = nil,
--       },
--       Sdk = {
--         -- Specifies whether to include preview versions of the .NET SDK when
--         -- determining which version to use for project loading.
--         IncludePrereleases = true,
--       },
--     },
--   })
-- end
