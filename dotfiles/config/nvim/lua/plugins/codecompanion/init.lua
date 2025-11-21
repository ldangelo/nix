return {
  {
    "olimorris/codecompanion.nvim", -- The KING of AI programming
    enabled = false,
    cmd = { "CodeCompanion", "CodeCompanionChat", "CodeCompanionActions" },
    dependencies = {
      "j-hui/fidget.nvim", -- Display status
      -- { "echasnovski/mini.pick", config = true },
      -- { "ibhagwan/fzf-lua", config = true },
    },
    opts = {
      ---@module "codecompanion"
      ---@type CodeCompanion.Config
      log_level = "DEBUG",
      adapters = {
        anthropic = function()
          return require("codecompanion.adapters").extend("anthropic", {
            env = {
              api_key = "ANTHROPIC_API_KEY",
            },
            schema = {
              model = {
                default = "claude-sonnet-4-20250514",
              },
            },
          })
        end,
        claude_code = function()
          local api_key = vim.fn.getenv("ANTHROPIC_API_KEY")
          if not api_key or api_key == "" then
            vim.notify(
              "ANTHROPIC_API_KEY environment variable is not set! Some AI features may not work.",
              vim.log.levels.WARN
            )
          end
          return require("codecompanion.adapters").extend("claude_code", {
            env = {
              ANTHROPIC_API_KEY = api_key,
            },
          })
        end,
        ollama = function()
          return require("codecompanion.adapters").extend("ollama", {
            schema = {
              model = {
                default = "qwen2.5-coder:latest",
              },
              num_ctx = {
                default = 16384,
              },
            },
          })
        end,
      },
      strategies = {
        chat = {
          adapter = "claude_code",
          roles = {
            llm = "CodeCompanion",
            user = "ldangelo",
          },
          keymaps = {
            send = {
              modes = {
                i = { "<C-CR>", "<C-s>" },
              },
            },
            completion = {
              modes = {
                i = "<C-x>",
              },
            },
          },
          slash_commands = {
            ["buffer"] = {
              keymaps = {
                modes = {
                  i = "<C-b>",
                },
              },
            },
            ["fetch"] = {
              keymaps = {
                modes = {
                  i = "<C-f>",
                },
              },
            },
            ["help"] = {
              opts = {
                max_lines = 1000,
              },
            },
            ["image"] = {
              keymaps = {
                modes = {
                  i = "<C-i>",
                },
              },
              opts = {
                dirs = { "~/Documents/Screenshots" },
              },
            },
          },
        },
        inline = {
          adapter = "anthropic",
        },
      },
      display = {
        action_palette = {
          provider = "default",
        },
        chat = {
          show_references = true,
          show_settings = true,
          icons = {
            tool_success = "󰸞 ",
            tool_error = "󰅙 ",
            tool_running = "󱥸 ",
          },
          fold_context = true,
        },
      },
    },
    keys = {
      {
        "<C-a>",
        "<cmd>CodeCompanionActions<CR>",
        desc = "Open the action palette",
        mode = { "n", "v" },
      },
      {
        "<Leader>a",
        "<cmd>CodeCompanionChat Toggle<CR>",
        desc = "Toggle a chat buffer",
        mode = { "n", "v" },
      },
      {
        "<LocalLeader>a",
        "<cmd>CodeCompanionChat Add<CR>",
        desc = "Add code to a chat buffer",
        mode = { "v" },
      },
      {
        "ga",
        "<cmd>CodeCompanionActions<CR>",
        desc = "CodeCompanion actions",
        mode = { "n", "v" },
      },
    },
    init = function()
      vim.cmd([[cab cc CodeCompanion]])
    end,
  },
}
