{ pkgs, config, lib, ... }:

let
  cfg = config.pi-agent;
  makeSettings = builtins.toJSON;
  builtInSkills = [
    ./pi-extensions/cavecrew
    ./pi-extensions/caveman
    ./pi-extensions/caveman-commit
    ./pi-extensions/caveman-review
    ./pi-extensions/caveman-compress
    ./pi-extensions/caveman-help
    ./pi-extensions/pi-help
    ./pi-extensions/caveman-stats
    ./pi-extensions/semantic-web-research
    ./pi-extensions/firecrawl-cli
    ./pi-extensions/exa-cli
    ./pi-extensions/sequentialthinking-cli
    ./pi-extensions/context7-cli
  ];
  enabledSkills = lib.unique (builtInSkills ++ cfg.skills);
  piVsCcDir = ./pi-extensions/pi-vs-cc;
  sandboxExtension = ./pi-extensions/sandbox;
  sandboxEnabled = builtins.any (e: e == sandboxExtension) cfg.extensions;
  managedExtensions = lib.filter (e: e != sandboxExtension) cfg.extensions;
  mcporterConfig = pkgs.writeText "mcporter.json" (makeSettings {
    "$schema" = "https://raw.githubusercontent.com/openclaw/mcporter/main/mcporter.schema.json";
    mcpServers = {
      docker = {
        description = "Docker MCP gateway for Firecrawl, Exa, Context7, Sequential Thinking, and other tools";
        baseUrl = "http://127.0.0.1:3100/mcp";
        lifecycle = {
          mode = "keep-alive";
        };
        headers = {
          Authorization = "Bearer \${MCP_DOCKER_BEARER_TOKEN}";
        };
      };
    };
  });

  installPackageCommands = lib.concatMapStrings (pkg:
  if lib.isString pkg then ''
    pi_cmd=""
    if command -v pi >/dev/null 2>&1; then
      pi_cmd="$(command -v pi)"
    elif [ -x /opt/homebrew/bin/pi ]; then
      pi_cmd=/opt/homebrew/bin/pi
    elif [ -x /usr/local/bin/pi ]; then
      pi_cmd=/usr/local/bin/pi
    else
      for candidate in "$HOME"/.nvm/versions/node/*/bin/pi; do
        if [ -x "$candidate" ]; then
          pi_cmd="$candidate"
          break
        fi
      done
    fi

    if [ -n "$pi_cmd" ]; then
      export PATH="$(dirname "$pi_cmd"):$PATH"
      "$pi_cmd" install ${lib.escapeShellArg pkg}
    else
      echo "pi not found; skipping package ${lib.escapeShellArg pkg}"
    fi
  '' else "") cfg.packages;
in {
  options.pi-agent = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Pi Agent configuration";
    };

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Settings for Pi Agent";
    };

    models = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Models configuration for Pi Agent";
    };

    binTools = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      description = "Binary tools to expose in ~/.pi/agent/bin";
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = "Pi Agent skills to install (directories containing SKILL.md)";
    };

    packages = lib.mkOption {
      type = lib.types.listOf (lib.types.oneOf [ lib.types.str lib.types.attrs ]);
      default = [];
      description = "Pi Agent packages to install, e.g. [ \"npm:pi-powerline-footer\" ]. Attrset entries are accepted for configs that also mirror local package objects into settings.json; activation only installs string specs.";
    };

    mcpConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Shared MCP configuration written to ~/.config/mcp/mcp.json for pi-mcp-adapter";
    };

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = "Pi Agent extensions to install (individual .ts files or directories with index.ts)";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = cfg.binTools;

      xdg.configFile = lib.mkMerge [
        (lib.mkIf (cfg.mcpConfig != {}) {
          "mcp/mcp.json" = {
            source = pkgs.writeText "mcp.json" (makeSettings cfg.mcpConfig);
          };
        })
        {
          "mcporter/mcporter.json" = {
            source = mcporterConfig;
            force = true;
          };
        }
      ];

      home.file = lib.mkMerge [
        {
          ".mcporter/mcporter.json" = {
            source = mcporterConfig;
            force = true;
          };
          ".pi/agent/settings.json" = {
            source = pkgs.writeText "pi-agent-settings.json" (makeSettings cfg.settings);
            force = true;
          };
          ".pi/agent/extensions/ask-user.ts".source = ./pi-extensions/ask-user.ts;
          ".pi/agent/extensions/tokens-per-second.ts".source = ./pi-extensions/tokens-per-second.ts;
          ".pi/agent/extensions/progressive-context.ts".source = ./pi-extensions/progressive-context.ts;
          ".pi/agent/extensions/auto-commit-on-exit.ts".source = ./pi-extensions/auto-commit-on-exit.ts;
          ".pi/agent/extensions/preset.ts".source = ./pi-extensions/preset.ts;
          ".pi/agent/extensions/nvim/index.ts".source = ./pi-extensions/nvim/index.ts;
          ".pi/agent/extensions/poly-notify/notify.json".source = ./pi-extensions/poly-notify/notify.json;
          ".pi/agent/extensions/subagent/index.ts".source = ./pi-extensions/subagent/index.ts;
          ".pi/agent/extensions/subagent/agents.ts".source = ./pi-extensions/subagent/agents.ts;
          ".pi/agent/agents" = { recursive = true; source = ./pi-extensions/subagent/agents; };
          ".pi/agent/prompts" = { recursive = true; source = ./pi-extensions/subagent/prompts; };
          ".pi/agent/vendor/pi-vs-cc" = { recursive = true; source = piVsCcDir; };

          ".local/bin/pi-chain" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/agent-chain/index.ts" "$@"
            '';
          };
          ".local/bin/pi-team" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/agent-team/index.ts" "$@"
            '';
          };
          ".local/bin/pi-system" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/system-select/index.ts" "$@"
            '';
          };
          ".local/bin/pi-tilldone" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/tilldone/index.ts" "$@"
            '';
          };
          ".local/bin/pi-coms" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/coms/index.ts" "$@"
            '';
          };
          ".local/bin/pi-coms-net" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/coms-net/index.ts" "$@"
            '';
          };
          ".local/bin/pi-coms-net-server" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              if ! command -v bun >/dev/null 2>&1; then
                echo "bun required for pi-coms-net-server" >&2
                exit 127
              fi
              exec bun "$HOME/.pi/agent/vendor/pi-vs-cc/scripts/coms-net-server.ts" "$@"
            '';
          };
          ".local/bin/pi-mcp-call" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-mcp-call; };
          ".local/bin/pi-firecrawl" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-firecrawl; };
          ".local/bin/pi-exa" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-exa; };
          ".local/bin/pi-context7" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-context7; };
          ".local/bin/pi-think" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-think; };
          ".local/bin/pi-vs-cc-agents-init" = {
            executable = true;
            text = ''
              #!/usr/bin/env bash
              set -euo pipefail
              target="''${1:-$PWD/.pi/agents}"
              mkdir -p "$target"
              cp -R "$HOME/.pi/agent/vendor/pi-vs-cc/agents/." "$target/"
              echo "Installed Pi agent-chain/team sample agents to $target"
            '';
          };
        }
        (builtins.listToAttrs (
          builtins.map (e: {
            name = ".pi/agent/extensions/${builtins.baseNameOf (builtins.toString e)}";
            value = { source = e; };
          }) managedExtensions
        ))
        (lib.mkIf (cfg.models != {}) {
          ".pi/agent/models.json".source =
            pkgs.writeText "pi-agent-models.json" (makeSettings cfg.models);
        })
        (lib.mkIf (cfg.binTools != []) {
          ".pi/agent/bin" = {
            recursive = true;
            executable = true;
            source = pkgs.runCommandLocal "pi-agent-bin" {} ''
              mkdir -p "$out"
              ${lib.concatMapStrings (tool: ''
                for bin in ${tool}/bin/*; do
                  ln -s "$bin" "$out/$(basename "$bin")"
                done
              '') cfg.binTools}
            '';
          };
        })
      ];
    }

    (lib.mkIf (cfg.packages != []) {
      home.activation.installPiAgentPackages =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Installing Pi Agent packages..."
          ${installPackageCommands}
        '';
    })

    (lib.mkIf (enabledSkills != []) {
      home.activation.installPiAgentSkills =
        lib.hm.dag.entryAfter [ "installPiAgentPackages" ] ''
          echo "Installing Pi Agent skills..."
          for skillPath in ${lib.concatMapStrings (s: "\"${lib.escapeShellArg s}\" ") enabledSkills}; do
            skillName=$(basename "$skillPath")
            skillTarget="$HOME/.pi/agent/skills/$skillName"
            if [ -e "$skillTarget" ]; then
              chmod -R u+rwX "$skillTarget" 2>/dev/null || true
              rm -rf "$skillTarget"
            fi
            mkdir -p "$skillTarget"
            cp -R "$skillPath"/. "$skillTarget"/
            chmod -R u+rwX "$skillTarget"
            # Make shell scripts executable
            find "$skillTarget" -name "*.sh" -exec chmod +x {} \;
          done
        '';
    })

    {
      home.activation.cleanupPiAgentExtensionBackups =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Cleaning stale Pi Agent extension backups..."
          for backup in \
            "$HOME/.pi/agent/extensions/nvim.bak" \
            "$HOME/.pi/agent/extensions/sandbox.bak" \
            "$HOME/.pi/agent/extensions/poly-notify.bak" \
            "$HOME/.pi/agent/extensions/auto-commit-on-exit.ts.bak" \
            "$HOME/.pi/agent/extensions/preset.ts.bak"; do
            if [ -e "$backup" ]; then
              chmod -R u+rwX "$backup" 2>/dev/null || true
              rm -rf "$backup"
            fi
          done
        '';
    }

    (lib.mkIf sandboxEnabled {
      home.activation.installSandboxDeps =
        lib.hm.dag.entryAfter [ "writeBoundary" "cleanupPiAgentExtensionBackups" "installPiAgentPackages" ] ''
          echo "Installing sandbox extension dependencies..."
          sandboxDir="$HOME/.pi/agent/extensions/sandbox"
          if [ -L "$sandboxDir" ] || [ -e "$sandboxDir" ]; then
            chmod -R u+rwX "$sandboxDir" 2>/dev/null || true
            rm -rf "$sandboxDir"
          fi
          mkdir -p "$sandboxDir"
          cp -R "${sandboxExtension}"/. "$sandboxDir"/
          chmod -R u+rwX "$sandboxDir"
          cd "$sandboxDir"
          npm install --omit=dev
        '';
    })
  ]);
}
