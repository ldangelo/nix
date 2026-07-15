# Pi Agent Configuration Module
#
# Provides centralized defaults for pi-agent settings, extensions, packages,
# and skills. Machine-specific config in flake.nix should only override what
# differs — see each machine's override block for the minimal pattern.

{ pkgs, config, lib, ... }:

let
  cfg = config.pi-agent;
  homeDir = config.home.homeDirectory;
  makeSettings = builtins.toJSON;

  # ── Default packages (NPM + local sources) ──────────────────────────────
  defaultPackages = [
    "npm:pi-powerline-footer"
    "npm:@hypabolic/pi-hypa"
    "npm:pi-context"
    "https://github.com/tmonk/pi-goal-x"
    "https://github.com/KristjanPikhof/pi-yaml-hooks"
    "npm:pi-subagents"
    "npm:pi-intercom"
    "npm:pi-memory"
    "npm:@raquezha/noheadroom"
  ];

  # Merge default packages with user-provided packages (dedup).
  # attrset entries (e.g. { source = "..."; extensions = []; }) are also included.
  mergedPackages = lib.unique (defaultPackages ++ cfg.packages);
  settingsPackages =
    if cfg.settings ? packages
    then lib.unique (mergedPackages ++ cfg.settings.packages)
    else mergedPackages;

  # ── Vault Mind shared Obsidian wiki ─────────────────────────────────────
  vaultMindVaultPath =
    if pkgs.stdenv.hostPlatform.isDarwin
    then "${homeDir}/Library/Mobile Documents/iCloud~md~obsidian/Documents/ldangelo"
    else "${homeDir}/.pi/agent/vault-mind/vault";
  vaultMindBase = "${vaultMindVaultPath}/Agent/VaultMind";
  vaultMindConfig = {
    version = 2;
    collections = {
      main = {
        path = "${vaultMindBase}/collections/main.jsonl";
        schema = [ "id" "domain" "source" "fact" "tag" "artifact" ];
        dedupField = "fact";
      };
      pending = {
        path = "${vaultMindBase}/collections/pending.jsonl";
        schema = "main";
      };
      worklog = {
        path = "${vaultMindBase}/collections/worklog.jsonl";
        schema = [ "id" "date" "agent" "project" "summary" "status" "bottlenecks" "tags" ];
        dedupField = "id";
      };
      decisions = {
        path = "${vaultMindBase}/collections/decisions.jsonl";
        schema = [ "id" "date" "project" "decision" "rationale" "status" "tags" ];
        dedupField = "decision";
      };
      context_events = {
        path = "${vaultMindBase}/collections/context_events.jsonl";
        schema = [ "id" "type" "session_entry_id" "content" "timestamp" "tags" ];
        dedupField = "id";
      };
    };
    injectors = [
      {
        name = "recall-topic";
        regex = "(?:recall|remember|wiki|knowledge)\\s+(.+)";
        collection = "main";
        filterField = "tag";
        artifactPath = "${vaultMindBase}/artifacts/recall.md";
      }
      {
        name = "decision-context";
        regex = "decision\\s+(.+)";
        collection = "decisions";
        filterField = "project";
        artifactPath = "${vaultMindBase}/artifacts/decisions.md";
      }
      {
        name = "bottleneck-context";
        regex = "bottleneck\\s+(.+)";
        collection = "worklog";
        filterField = "project";
        artifactPath = "${vaultMindBase}/artifacts/bottlenecks.md";
      }
    ];
    vaultMind = {
      dataDir = "${homeDir}/.pi/agent/vault-mind/lancedb";
      embedding = {
        useTransformers = true;
        localUrl = "http://127.0.0.1:11434";
      };
      graph = {
        enabled = true;
        canvasSync = false;
      };
      ftsEnabled = true;
      httpPort = 11435;
      autoIndex = false;
      vaults = {
        default = {
          path = vaultMindVaultPath;
          autoSync = true;
          autoSyncTags = [ "decision" "insight" "requirement" "bottleneck" "worklog" ];
          autoSyncMinLength = 200;
        };
      };
    };
    extensionCompatibility = {
      pi-context = {
        enabled = true;
        tagPatterns = [];
        enhanceInjectors = false;
        autoEnableAcm = true;
        indexContextEvents = true;
      };
    };
  };

  # ── Default settings ────────────────────────────────────────────────────
  defaultSettings = {
    defaultProvider = "openai-codex";
    defaultModel = "gpt-5.5";
    defaultThinkingLevel = "medium";
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
      packages = settingsPackages;
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
  runtimeBinTools = lib.unique ([ pkgs.git pkgs.nodejs ] ++ cfg.binTools);

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
          export PATH="${pkgs.git}/bin:${pkgs.nodejs}/bin:$(dirname "$pi_cmd"):$PATH"
          "$pi_cmd" install "$pkg" || echo "pi install $pkg failed; package remains in settings for startup reconciliation"
        else
          echo "pi not found; skipping package $pkg"
        fi
      fi
    '' else "") settingsPackages;

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
      home.packages = runtimeBinTools;

      home.sessionVariables = {
        PI_OBSIDIAN_VAULT = vaultMindVaultPath;
        PI_MEMORY_DIR = "${vaultMindVaultPath}/Agent/PiMemory";
        PI_AUTO_COMPACT_THRESHOLD_PERCENT = "60";
      };
      programs.zsh.envExtra = lib.mkAfter ''
        export PI_OBSIDIAN_VAULT=${lib.escapeShellArg vaultMindVaultPath}
        export PI_MEMORY_DIR=${lib.escapeShellArg "${vaultMindVaultPath}/Agent/PiMemory"}
        export PI_AUTO_COMPACT_THRESHOLD_PERCENT=60
      '';

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
          ".pi/agent/keybindings.json" = {
            source = pkgs.writeText "pi-agent-keybindings.json" (makeSettings {
              # Ctrl-p / Ctrl-n browse prompt history via editor up/down.
              # Unbind default Ctrl-p/Ctrl-n app actions so prompt history wins.
              "tui.editor.cursorUp" = [ "up" "ctrl+p" ];
              "tui.editor.cursorDown" = [ "down" "ctrl+n" ];
              "app.model.cycleForward" = [];
              "app.session.togglePath" = [];
              "app.session.toggleNamedFilter" = [];
              "app.models.toggleProvider" = [];
            });
            force = true;
          };
          ".pi/agent/vault-mind.config.json" = {
            source = pkgs.writeText "vault-mind.config.json" (makeSettings vaultMindConfig);
            force = true;
          };
          ".pi/agent/headroom/settings.json" = {
            source = pkgs.writeText "pi-headroom-settings.json" (makeSettings {
              enabled = true;
              baseUrl = "http://127.0.0.1:8788";
              autoStart = true;
              mode = "quiet";
              minContextTokens = 10000;
              minMessageChars = 2000;
            });
            force = true;
          };

          # ── Built-in extensions ──────────────────────────────────────────
          ".pi/agent/extensions/ask-user.ts".source = ./pi-extensions/ask-user.ts;
          ".pi/agent/extensions/tokens-per-second.ts".source = ./pi-extensions/tokens-per-second.ts;
          ".pi/agent/extensions/progressive-context.ts".source = ./pi-extensions/progressive-context.ts;
          ".pi/agent/extensions/auto-commit-on-exit.ts".source = ./pi-extensions/auto-commit-on-exit.ts;
          ".pi/agent/extensions/preset.ts".source = ./pi-extensions/preset.ts;
          ".pi/agent/extensions/auto-compact.ts".source = ./pi-extensions/auto-compact.ts;
          ".pi/agent/extensions/pi-fact.ts".source = ./pi-extensions/pi-fact.ts;
          ".pi/agent/extensions/obsidian-session-saver.ts".source = ./pi-extensions/obsidian/obsidian-session-saver.ts;
          ".pi/agent/extensions/nvim/index.ts".source = ./pi-extensions/nvim/index.ts;
          ".pi/agent/extensions/poly-notify/notify.json".source = ./pi-extensions/poly-notify/notify.json;

          # ── Vendor packages ──────────────────────────────────────────────
          ".pi/agent/vendor/pi-vs-cc" = { recursive = true; source = piVsCcDir; };

          # ── Local bin wrappers ───────────────────────────────────────────
          ".local/bin/hypa" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              exec "$HOME/.pi/agent/npm/node_modules/.bin/hypa" "$@"'';
          };
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
          ".local/bin/pi-notify" = {
            executable = true;
            text = ''#!/usr/bin/env bash
              set -euo pipefail
              title="''${1:-Pi}"
              body="''${2:-Ready for input}"

              # tmux catches BEL via alert-bell and emits session/window-aware notification.
              printf '\a'

              if command -v terminal-notifier >/dev/null 2>&1; then
                terminal-notifier \
                  -title "$title" \
                  -message "$body" \
                  -sound default \
                  -group "pi-agent" >/dev/null 2>&1 || true
              elif [ -n "''${KITTY_WINDOW_ID:-}" ]; then
                printf '\033]99;i=1:d=0;%s\033\\' "$title"
                printf '\033]99;i=1:p=body;%s\033\\' "$body"
              else
                printf '\033]777;notify;%s;%s\a' "$title" "$body"
              fi
            '';
          };

          ".pi/agent/hook/hooks.yaml" = {
            force = true;
            text = ''
              hooks:
                - id: pi-notify-on-idle
                  event: session.idle
                  actions:
                    - bash: '"$HOME/.local/bin/pi-notify" "Pi" "Ready for input"'
            '';
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
        (lib.mkIf (runtimeBinTools != []) {
          ".pi/agent/bin" = {
            recursive = true;
            executable = true;
            source = pkgs.runCommandLocal "pi-agent-bin" {} ''
              mkdir -p "$out"
              ${lib.concatMapStrings (tool: ''
                for bin in ${tool}/bin/*; do
                  ln -s "$bin" "$out/$(basename "$bin")"
                done
              '') runtimeBinTools}
            '';
          };
        })
      ];
    }

    # ── Vault Mind directory bootstrap ─────────────────────────────────────
    {
      home.activation.setupVaultMind =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Setting up Vault Mind directories..."
          mkdir -p ${lib.escapeShellArg "${vaultMindBase}/collections"} \
                   ${lib.escapeShellArg "${vaultMindBase}/artifacts"} \
                   ${lib.escapeShellArg "${vaultMindVaultPath}/Agent/PiMemory/daily"} \
                   "$HOME/.pi/agent/vault-mind/lancedb"
        '';
    }

    # ── Headroom runtime bootstrap ───────────────────────────────────────
    {
      home.activation.installHeadroomRuntime =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Installing Headroom runtime..."
          if ! ${pkgs.pipx}/bin/pipx list --short 2>/dev/null | grep -q '^headroom-ai '; then
            ${pkgs.pipx}/bin/pipx install --include-deps 'headroom-ai[all]'
          else
            ${pkgs.pipx}/bin/pipx runpip headroom-ai install --upgrade 'headroom-ai[all]' || true
          fi
          rm -f "$HOME/.pi/agent/extensions/headroom-mcp.ts" "$HOME/.pi/agent/extensions/headroom-mcp.ts.bak"
        '';
    }

    # ── Package installation activation ─────────────────────────────────────
    (lib.mkIf (settingsPackages != []) {
      home.activation.installPiAgentPackages =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Installing Pi Agent packages..."
          ${installPackageCommands}
        '';
    })

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
            "$HOME/.pi/agent/extensions/agentmemory" \
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
