# OMP (Oh My Pi) Configuration Module
#
# Migrates relevant settings from pi-agent and configures:
#   - OMP config.yml (merged pi -> OMP settings)
#   - MCP servers: hindsight, agentmemory, headroom
#   - OMP marketplace for skills
#   - Headroom settings
#   - Ensemble skill installation

{ pkgs, lib, config, ... }:

let
  cfg = config.omp;
  homeDir = config.home.homeDirectory;
  ompAgentDir = "${homeDir}/.omp/agent";
  ompDir = "${homeDir}/.omp";
  ompMarketplaceUrl = "https://github.com/oh-my-pi/marketplace";

  # Read pi-agent settings for migration
  piSettingsPath = "${homeDir}/.pi/agent/settings.json";
  piHeadroomSettingsPath = "${homeDir}/.pi/agent/headroom/settings.json";

  # Use builtins.readFile instead of lib.importJSON to avoid pure-mode issues
  piSettings =
    if builtins.pathExists piSettingsPath
    then builtins.fromJSON (builtins.readFile piSettingsPath)
    else {};
  piHeadroomSettings =
    if builtins.pathExists piHeadroomSettingsPath
    then builtins.fromJSON (builtins.readFile piHeadroomSettingsPath)
    else {};
  # Read existing OMP config.yml for deep-merge
  # approvalMode migration: pi "low" + "ask" -> OMP "write"
  approvalMode = "write";
  # Read existing OMP config.yml for deep-merge
  existingOmpConfigPath = "${ompAgentDir}/config.yml";
  existingOmpConfig =
    if builtins.pathExists existingOmpConfigPath
    then builtins.fromJSON (builtins.readFile existingOmpConfigPath)
    else {};
  # Headroom MCP server config from pi-agent headroom settings
  headroomMcpConfig = {
    autoStart = piHeadroomSettings.autoStart or true;
    baseUrl = piHeadroomSettings.baseUrl or "http://127.0.0.1:8788";
    minContextTokens = piHeadroomSettings.minContextTokens or 10000;
    minMessageChars = piHeadroomSettings.minMessageChars or 2000;
    mode = piHeadroomSettings.mode or "quiet";
  };
  # Build merged config.yml attrs
  mergedOmpConfig =
    lib.recursiveUpdate existingOmpConfig
      ({
        modelRoles = {
          default = piSettings.defaultModel or "minimax/MiniMax-M2.7";
        } // lib.optionalAttrs (existingOmpConfig ? modelRoles) existingOmpConfig.modelRoles;
        defaultThinkingLevel = piSettings.defaultThinkingLevel or "medium";
        symbolPreset =
          piSettings.powerline.powerlinePreset
          or piSettings.powerline.preset
          or existingOmpConfig.symbolPreset
          or "nerd";
        tools = { approvalMode = approvalMode; };
        headroom = headroomMcpConfig;
        skills = {
          customDirectories =
            ["${homeDir}/.pi/agent/skills"]
            ++ (existingOmpConfig.skills.customDirectories or []);
        };
        memory.backend = "hindsight";
        hindsight.mentalModelAutoSeed = true;
        compaction.thresholdPercent =
          existingOmpConfig.compaction.thresholdPercent or 70;
        retry.fallbackChains = {
          default = [
            "${piSettings.defaultProvider or "openai-codex"}/gpt-5.4"
          ];
        };
      });

  # Build mcp.json attrs
  existingMcp =
    let mcpPath = "${ompAgentDir}/mcp.json";
    in if builtins.pathExists mcpPath
       then builtins.fromJSON (builtins.readFile mcpPath)
       else { mcpServers = {}; };

  mcpJson =
    let
      base = {
        headroom = {
          command = "npx";
          args = [ "-y" "@raquezha/noheadroom" ];
          env = {
            HEADROOM_BASE_URL = headroomMcpConfig.baseUrl;
            HEADROOM_AUTO_START = if headroomMcpConfig.autoStart then "true" else "false";
          } // lib.optionalAttrs (headroomMcpConfig.minContextTokens != null) {
            HEADROOM_MIN_CONTEXT_TOKENS = toString headroomMcpConfig.minContextTokens;
          } // lib.optionalAttrs (headroomMcpConfig.minMessageChars != null) {
            HEADROOM_MIN_MESSAGE_CHARS = toString headroomMcpConfig.minMessageChars;
          };
        };
        agentmemory = {
          command = "npx";
          args = [ "-y" "@agentmemory/agentmemory" ];
        };
        hindsight = {
          command = "npx";
          args = [ "-y" "@vectorize-io/hindsight-mcp" ];
        };
      };
      extra = cfg.extraMcpServers;
    in {
      mcpServers = existingMcp.mcpServers // base // extra;
    };

in {
  options.omp = with lib; {
    enable = mkEnableOption "Oh My Pi (OMP) agent configuration";

    extraMcpServers = mkOption {
      type = types.attrsOf (types.submodule {
        options = with lib; {
          command = mkOption { type = types.str; };
          args = mkOption { type = types.listOf types.str; default = []; };
          env = mkOption { type = types.attrsOf types.str; default = {}; };
          type = mkOption { type = types.str; default = "stdio"; };
        };
      });
      default = {};
      description = "Extra MCP servers to add to mcp.json";
    };

    extraSkills = mkOption {
      type = types.listOf types.path;
      default = [];
      description = "Paths to additional skill directories to install";
    };
  };

  config = lib.mkIf cfg.enable {

    # ── config.yml ─────────────────────────────────────────────────────
    home.file."${ompAgentDir}/config.yml".text = builtins.toJSON mergedOmpConfig;

    # ── mcp.json ───────────────────────────────────────────────────────
    home.file."${ompAgentDir}/mcp.json".text = builtins.toJSON mcpJson;

    # ── marketplaces.json ───────────────────────────────────────────────
    home.file."${ompDir}/marketplaces.json".text =
      builtins.toJSON [{ url = ompMarketplaceUrl; type = "git"; }];

    # ── Activation scripts ────────────────────────────────────────────────

    home.activation.ompMarketplace = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      echo "Installing OMP marketplace..."
      mkdir -p "${ompDir}"
      if [ ! -f "${ompDir}/marketplaces.json" ]; then
        ${pkgs.curl}/bin/curl -fsSL \
          "https://raw.githubusercontent.com/oh-my-pi/marketplace/main/marketplace.json" \
          -o "${ompDir}/marketplaces.json" 2>/dev/null \
          || echo '[{"url":"${ompMarketplaceUrl}","type":"git"}]' \
             > "${ompDir}/marketplaces.json"
      fi
    '';

    home.activation.ompEnsembleSkill = lib.hm.dag.entryAfter [ "ompMarketplace" ] ''
      echo "Installing ensemble skill into OMP..."
      local skillTarget="${ompAgentDir}/skills/ensemble"
      if [ ! -d "$skillTarget" ]; then
        mkdir -p "${ompAgentDir}/skills"
        # Sparse-clone just the ensemble skill
        ${pkgs.git}/bin/git clone \
          --depth 1 \
          --filter=blob:none \
          --sparse \
          "https://github.com/oh-my-pi/marketplace.git" \
          "${ompDir}/.marketplace-tmp" 2>/dev/null \
          || true
        if [ -d "${ompDir}/.marketplace-tmp" ]; then
          ${pkgs.git}/bin/git -C "${ompDir}/.marketplace-tmp" \
            sparse-checkout set "skills/ensemble" 2>/dev/null || true
          if [ -f "${ompDir}/.marketplace-tmp/skills/ensemble/SKILL.md" ]; then
            mkdir -p "$skillTarget"
            cp -R "${ompDir}/.marketplace-tmp/skills/ensemble/." "$skillTarget/"
          fi
          rm -rf "${ompDir}/.marketplace-tmp"
        fi
        # Fallback placeholder if clone failed
        if [ ! -f "$skillTarget/SKILL.md" ]; then
          mkdir -p "$skillTarget"
          cat > "$skillTarget/SKILL.md" << 'SKILLEOF'
---
name: ensemble
description: Ensemble workflow — coordinate multiple specialized agents for complex tasks
---

# Ensemble Skill

Coordinate multiple specialized agents to tackle complex, multi-domain tasks.

## When to use

- Task requires backend + frontend + infrastructure expertise simultaneously
- Code review spanning multiple languages or frameworks
- Architectural decisions with cross-cutting concerns
- Research tasks requiring parallel investigation streams

## Workflow

1. **Decompose** — identify distinct expertise domains
2. **Parallel dispatch** — spawn specialist agents via `task` tool
3. **Synthesize** — merge findings into coherent solution

## Tips

- Assign one specialist per domain
- Share context constraints in each sub-agent prompt
- Use `agent://<id>` to reference sub-agent outputs
SKILLEOF
        fi
      fi
    '';

    home.activation.ompExtraSkills = lib.hm.dag.entryAfter [ "ompEnsembleSkill" ] ''
      ${lib.optionalString (cfg.extraSkills != []) ''
        echo "Installing extra OMP skills..."
        ${lib.concatMapStrings (s: ''
          skillName=$(basename '${s}')
          skillTarget="${ompAgentDir}/skills/$skillName"
          if [ -e "$skillTarget" ]; then
            chmod -R u+rwX "$skillTarget" 2>/dev/null || true
            rm -rf "$skillTarget"
          fi
          mkdir -p "$skillTarget"
          cp -R '${s}'/. "$skillTarget/" 2>/dev/null || true
          chmod -R u+rwX "$skillTarget"
        '') cfg.extraSkills}
      ''}
    '';
  };
}
