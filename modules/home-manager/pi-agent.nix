{ pkgs, config, lib, ... }:

let
  cfg = config.pi-agent;
  makeSettings = pkgs.lib.generators.toJSON {};

  installPackageCommands = lib.concatMapStrings (pkg: ''
    pi_cmd=""
    if command -v pi >/dev/null 2>&1; then
      pi_cmd="$(command -v pi)"
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
  '') cfg.packages;
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

    packages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description = "Pi Agent packages to install, e.g. [ \"npm:pi-powerline-footer\" ]";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      home.packages = cfg.binTools;

      home.file.".pi/agent/settings.json".source =
        pkgs.writeText "pi-agent-settings.json" (makeSettings cfg.settings);

      home.file.".pi/agent/extensions/ask-user.ts".source = ./pi-extensions/ask-user.ts;
      home.file.".pi/agent/extensions/subagent/index.ts".source = ./pi-extensions/subagent/index.ts;
      home.file.".pi/agent/extensions/subagent/agents.ts".source = ./pi-extensions/subagent/agents.ts;
      home.file.".pi/agent/agents" = {
        recursive = true;
        source = ./pi-extensions/subagent/agents;
      };
      home.file.".pi/agent/prompts" = {
        recursive = true;
        source = ./pi-extensions/subagent/prompts;
      };
    }

    (lib.mkIf (cfg.models != {}) {
      home.file.".pi/agent/models.json".source =
        pkgs.writeText "pi-agent-models.json" (makeSettings cfg.models);
    })

    (lib.mkIf (cfg.binTools != []) {
      home.file.".pi/agent/bin" = {
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

    (lib.mkIf (cfg.packages != []) {
      home.activation.installPiAgentPackages =
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          echo "Installing Pi Agent packages..."
          ${installPackageCommands}
        '';
    })
  ]);
}
