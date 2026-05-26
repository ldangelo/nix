{ pkgs, config, lib, ... }:

let
  cfg = config.context-mode;
in {
  options.context-mode = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Configure the context-mode MCP server for Claude Code, Codex CLI,
        and OpenCode. The binary is launched on demand via `npx -y context-mode`
        so no global npm install is required. pi-agent is configured separately
        via pi-agent.packages.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."opencode/opencode.json".text = builtins.toJSON {
      "$schema" = "https://opencode.ai/config.json";
      plugin = [ "context-mode" ];
    };

    home.file.".codex/config.toml".text = ''
      [mcp_servers.context-mode]
      command = "npx"
      args = ["-y", "context-mode"]
    '';

    home.activation.registerContextModeClaude =
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        claude_cfg="$HOME/.claude.json"
        if [ -f "$claude_cfg" ] && grep -q '"context-mode"' "$claude_cfg"; then
          : # already registered
        elif command -v claude >/dev/null 2>&1; then
          echo "Registering context-mode MCP server with claude..."
          claude mcp add --scope user context-mode -- npx -y context-mode || \
            echo "claude mcp add failed (non-fatal)"
        else
          echo "claude not in PATH; skipping context-mode MCP registration"
        fi
      '';
  };
}
