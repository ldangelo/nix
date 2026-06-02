# Pi Agent Configuration Module
#
# Provides centralized defaults for pi-agent settings, extensions, packages,
# and skills. Machine-specific config in flake.nix should only override what
# differs — see each machine's override block for the minimal pattern.

{ pkgs, config, lib, ... }:

let
  cfg = config.pi-agent;
  makeSettings = builtins.toJSON;

  # ── Default packages (NPM + local sources) ──────────────────────────────
  defaultPackages = [
    "npm:pi-powerline-footer"
    "npm:pi-hooks"
    # "npm:pi-context"  # disabled - API incompatible with pi-coding-agent 0.78.0
    "npm:pi-subagents"
    "npm:pi-intercom"
    "npm:context-mode"
  ];

  # Merge default packages with user-provided packages (dedup).
  # attrset entries (e.g. { source = "..."; extensions = []; }) are also included.
  mergedPackages = defaultPackages ++ cfg.packages;

  # ── Default settings ────────────────────────────────────────────────────
  defaultSettings = {
    lastChangelogVersion = "0.72.1";
    defaultProvider = "litellm";
    defaultModel = "coding";
    defaultThinkingLevel = "medium";
    packages = mergedPackages;
    powerline = { preset = "nerd"; };
    workingVibeMode = "file";
    workingVibe = "off";
    permissionLevel = "low";
    permissionMode = "ask";
    bashMode = {
      toggleShortcut = "ctrl+shift+b";
      transcriptMaxLines = 2000;
      transcriptMaxBytes = 524288;
    };
  };

  # Build final settings: merge defaults with machine overrides.
  # Only the "packages" field needs special handling (append user packages).
  builtSettings =
    let
      # Merge settings: defaults + user overrides, with package list combined
      merged = defaultSettings // cfg.settings;
    in
    merged // {
      packages = if cfg.settings ? packages
        then mergedPackages ++ cfg.settings.packages
        else mergedPackages;
    };

  # ── Built-in skills ─────────────────────────────────────────────────────
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
    ./pi-extensions/assess-repo
    ./pi-extensions/assess-purpose
    ./pi-extensions/assess-architect
    ./pi-extensions/assess-code-quality
    ./pi-extensions/assess-testing
    ./pi-extensions/assess-security
    ./pi-extensions/assess-ai-readiness
    ./pi-extensions/assess-cicd
    ./pi-extensions/assess-team
    ./pi-extensions/assess-team-code-quality
    ./pi-extensions/assess-team-test-coverage
    ./pi-extensions/assess-team-ai-adoption
    ./pi-extensions/assess-team-documentation
    ./pi-extensions/assess-team-incidents
    ./pi-extensions/assess-team-code-review
    ./pi-extensions/obsidian
  ];
  enabledSkills = lib.unique (builtInSkills ++ cfg.skills);

  # ── Extension sources (hardcoded + user-provided) ──────────────────────
  piVsCcDir = ./pi-extensions/pi-vs-cc;
  sandboxExtension = ./pi-extensions/sandbox;
  sandboxEnabled = builtins.any (e: e == sandboxExtension) cfg.extensions;
  managedExtensions = lib.filter (e: e != sandboxExtension) cfg.extensions;

  # ── MC Porter config ───────────────────────────────────────────────────
  mcporterConfig = pkgs.writeText "mcporter.json" (makeSettings {
    "$schema" = "https://raw.githubusercontent.com/openclaw/mcporter/main/mcporter.schema.json";
    mcpServers = {
      docker = {
        description = "Docker MCP gateway for Firecrawl, Exa, Context7, Sequential Thinking, and other tools";
        baseUrl = "http://127.0.0.1:3100/mcp";
        lifecycle.mode = "keep-alive";
        headers.Authorization = "Bearer \${MCP_DOCKER_BEARER_TOKEN}";
      };
    };
  });

  # ── Package installation activation script ──────────────────────────────
  installPackageCommands = lib.concatMapStrings (pkg:
    if lib.isString pkg then ''
      pkg=${lib.escapeShellArg pkg}
      if [[ "$pkg" == npm:* ]]; then
        npm_pkg="''${pkg#npm:}"
        mkdir -p "$HOME/.pi/agent/npm"
        if [ ! -f "$HOME/.pi/agent/npm/package.json" ]; then
          printf '{"name":"pi-extensions","private":true,"dependencies":{}}\n' > "$HOME/.pi/agent/npm/package.json"
        fi
        PATH="${pkgs.nodejs}/bin:$PATH" ${pkgs.nodejs}/bin/npm --prefix "$HOME/.pi/agent/npm" install "$npm_pkg"
      else
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
          "$pi_cmd" install "$pkg"
        else
          echo "pi not found; skipping package $pkg"
        fi
      fi
    '' else "") cfg.packages;

in
{
  # ── Module options ──────────────────────────────────────────────────────
  options.pi-agent = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable Pi Agent configuration";
    };

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Machine-specific overrides for Pi Agent settings (merged with defaults)";
    };

    models = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Models configuration for Pi Agent (from pi-models.json)";
    };

    binTools = lib.mkOption {
      type = lib.types.listOf lib.types.package;
      default = [];
      description = "Binary tools to expose in ~/.pi/agent/bin";
    };

    skills = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = "Additional Pi Agent skills to install (merged with built-in skills)";
    };

    packages = lib.mkOption {
      type = lib.types.listOf (lib.types.oneOf [ lib.types.str lib.types.attrs ]);
      default = [];
      description = "Extra Pi Agent packages to install alongside defaults (e.g. { source = \"/path\"; extensions = []; })";
    };

    mcpConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Shared MCP configuration written to ~/.config/mcp/mcp.json for pi-mcp-adapter";
    };

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.path;
      default = [];
      description = "Additional Pi Agent extensions to install (merged with built-in extensions)";
    };

    ensembleSource = lib.mkOption {
      type = lib.types.path;
      default = ./pi-extensions/ensemble;
      defaultText = lib.literalMD "local path (override with fetchFromGitHub result)";
      description = "Path to the Ensemble Pi package (typically from fetchFromGitHub)";
    };
  };

  # ── Module config ───────────────────────────────────────────────────────
  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = cfg.binTools;

      xdg.configFile = lib.mkMerge [
        (lib.mkIf (cfg.mcpConfig != {}) {
          "mcp/mcp.json" = { source = pkgs.writeText "mcp.json" (makeSettings cfg.mcpConfig); };
        })
        { "mcporter/mcporter.json" = { source = mcporterConfig; force = true; }; }
      ];

      home.file = lib.mkMerge [
        {
          # ── MC Porter ────────────────────────────────────────────────────
          ".mcporter/mcporter.json" = { source = mcporterConfig; force = true; };

          # ── Core config files ────────────────────────────────────────────
          ".pi/agent/settings.json" = {
            source = pkgs.writeText "pi-agent-settings.json" (makeSettings builtSettings);
            force = true;
          };

          # ── Built-in extensions ──────────────────────────────────────────
          ".pi/agent/extensions/ask-user.ts".source = ./pi-extensions/ask-user.ts;
          ".pi/agent/extensions/tokens-per-second.ts".source = ./pi-extensions/tokens-per-second.ts;
          ".pi/agent/extensions/progressive-context.ts".source = ./pi-extensions/progressive-context.ts;
          ".pi/agent/extensions/auto-commit-on-exit.ts".source = ./pi-extensions/auto-commit-on-exit.ts;
          ".pi/agent/extensions/preset.ts".source = ./pi-extensions/preset.ts;
          ".pi/agent/extensions/nvim/index.ts".source = ./pi-extensions/nvim/index.ts;
          ".pi/agent/extensions/poly-notify/notify.json".source = ./pi-extensions/poly-notify/notify.json;

          # ── Vendor packages ──────────────────────────────────────────────
          ".pi/agent/vendor/pi-vs-cc" = { recursive = true; source = piVsCcDir; };

          # ── Local bin wrappers ───────────────────────────────────────────
          ".local/bin/pi-chain" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/agent-chain/index.ts" "$@"'';
          };
          ".local/bin/pi-team" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/agent-team/index.ts" "$@"'';
          };
          ".local/bin/pi-system" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/system-select/index.ts" "$@"'';
          };
          ".local/bin/pi-tilldone" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/tilldone/index.ts" "$@"'';
          };
          ".local/bin/pi-coms" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/coms/index.ts" "$@"'';
          };
          ".local/bin/pi-coms-net" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec pi -e "$HOME/.pi/agent/vendor/pi-vs-cc/coms-net/index.ts" "$@"'';
          };
          ".local/bin/pi-coms-net-server" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              if ! command -v bun >/dev/null 2>&1; then
                echo "bun required for pi-coms-net-server" >&2
                exit 127
              fi
              exec bun "$HOME/.pi/agent/vendor/pi-vs-cc/scripts/coms-net-server.ts" "$@"'';
          };
          ".local/bin/pi-mcp-call" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-mcp-call; };
          ".local/bin/pi-firecrawl" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-firecrawl; };
          ".local/bin/pi-exa" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-exa; };
          ".local/bin/pi-context7" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-context7; };
          ".local/bin/pi-think" = { executable = true; source = ./pi-extensions/mcp-lite/bin/pi-think; };
          ".local/bin/pi-vs-cc-agents-init" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              set -euo pipefail
              target="''${1:-$PWD/.pi/agents}"
              mkdir -p "$target"
              cp -R "$HOME/.pi/agent/vendor/pi-vs-cc/agents/." "$target/"
              echo "Installed Pi agent-chain/team sample agents to $target"'';
          };
        }

        # ── User-provided extensions ───────────────────────────────────────
        (builtins.listToAttrs (
          builtins.map (e: {
            name = ".pi/agent/extensions/${builtins.baseNameOf (builtins.toString e)}";
            value = { source = e; };
          }) managedExtensions
        ))

        # ── Models ─────────────────────────────────────────────────────────
        (lib.mkIf (cfg.models != {}) {
          ".pi/agent/models.json".source =
            pkgs.writeText "pi-agent-models.json" (makeSettings cfg.models);
        })

        # ── Bin tools symlinks ─────────────────────────────────────────────
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

    # ── Package installation activation ─────────────────────────────────────
    (lib.mkIf (cfg.packages != []) {
      home.activation.installPiAgentPackages =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Installing Pi Agent packages..."
          ${installPackageCommands}
        '';
    })

    # ── ACM patch (always runs) ───────────────────────────────────────────
    {
      home.activation.patchPiContextAutoAcm =
        lib.hm.dag.entryAfter [ "installPiAgentPackages" ] ''
          pi_context="$HOME/.pi/agent/npm/node_modules/pi-context/src/index.ts"
          if [ -f "$pi_context" ] && ! grep -q "auto-enable ACM on session start" "$pi_context"; then
            ${pkgs.python3}/bin/python3 - "$pi_context" <<'PY'
import pathlib, sys
path = pathlib.Path(sys.argv[1])
text = path.read_text()
needle = "export default function (pi: ExtensionAPI) {\n"
insert = "\n".join([
    "export default function (pi: ExtensionAPI) {",
    "    // auto-enable ACM on session start (Nix managed)",
    "    pi.on(\"session_start\", async (_event, ctx) => {",
    "        CommandCtx = ctx as unknown as ExtensionCommandContext;",
    "        if (_event.reason === \"startup\" || _event.reason === \"new\" || _event.reason === \"resume\" || _event.reason === \"fork\") {",
    "            pi.sendMessage({",
    "                customType: \"pi-context\",",
    "                content: \"use context-management skill\",",
    "                display: false,",
    "            }, {",
    "                deliverAs: \"followUp\"",
    "            });",
    "        }",
    "    });",
    "",
])
if needle not in text:
    raise SystemExit("pi-context patch target not found")
path.write_text(text.replace(needle, insert, 1))
PY
          fi
        '';
    }

    # ── Skills installation ───────────────────────────────────────────────
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
            find "$skillTarget" -name "*.sh" -exec chmod +x {} \;
          done
        '';
    })

    # ── Extension backup cleanup ──────────────────────────────────────────
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

    # ── Sandbox extension (if user adds it to extensions) ─────────────────
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
          ${pkgs.nodejs}/bin/npm install --omit=dev
        '';
    })
  ]);
}
