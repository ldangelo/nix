# OMP (Oh My Pi) Configuration Module
#
# Manages ~/.omp/agent/{config.yml,models.yml,mcp.json} and ~/.omp/marketplaces.json.
#
# Important: these files are materialized as REGULAR files at activation,
# not symlinked to /nix/store. OMP writes flock(2) lock files next to each
# config (e.g. config.yml.lock) via @oh-my-pi/pi-utils/file-lock.ts — which
# uses path.resolve() that follows symlinks — so a symlink would push the
# lock path into /nix/store (read-only) → EACCES at startup.

{ pkgs, lib, config, ... }:

let
  cfg = config.omp;
  homeDir = config.home.homeDirectory;
  ompAgentDir = "${homeDir}/.omp/agent";
  ompDir = "${homeDir}/.omp";
  ompMarketplaceUrl = "https://github.com/oh-my-pi/marketplace";

  # One-time migration inputs from pi-agent's settings files. Reads are still
  # state-dependent (depend on disk state of ~/.pi/agent/* at eval time), but
  # these files are NOT managed by this module — they're inputs, not outputs.
  piSettingsPath = "${homeDir}/.pi/agent/settings.json";
  piHeadroomSettingsPath = "${homeDir}/.pi/agent/headroom/settings.json";
  piModelsPath = ../../pi-models.json;
  piModels =
    if builtins.pathExists piModelsPath
    then builtins.fromJSON (builtins.readFile piModelsPath)
    else {};
  piSettings =
    if builtins.pathExists piSettingsPath
    then builtins.fromJSON (builtins.readFile piSettingsPath)
    else {};
  piHeadroomSettings =
    if builtins.pathExists piHeadroomSettingsPath
    then builtins.fromJSON (builtins.readFile piHeadroomSettingsPath)
    else {};
  # Headroom MCP server config from pi-agent headroom settings
  headroomMcpConfig = {
    autoStart = piHeadroomSettings.autoStart or true;
    baseUrl = piHeadroomSettings.baseUrl or "http://127.0.0.1:8788";
    minContextTokens = piHeadroomSettings.minContextTokens or 10000;
    minMessageChars = piHeadroomSettings.minMessageChars or 2000;
    mode = piHeadroomSettings.mode or "quiet";
  };
  # Defaults baked into module policy. NOT a deep-merge of any deployed file.
  # Runtime edits to ~/.omp/agent/config.yml are overwritten on next deploy —
  # that's by design. cfg.settings lets the user layer overrides explicitly
  # via flake.nix; nothing is read from managed output at eval time.
  defaults = {
    modelRoles.default = piSettings.defaultModel or "minimax/MiniMax-M2.7";
    defaultThinkingLevel = piSettings.defaultThinkingLevel or "medium";
    symbolPreset =
      piSettings.powerline.powerlinePreset
      or piSettings.powerline.preset
      or "nerd";
    tools.approvalMode = "write";  # module policy: do not weaken silently
    headroom = {
      autoStart = headroomMcpConfig.autoStart;
      baseUrl = headroomMcpConfig.baseUrl;
      mode = headroomMcpConfig.mode;
    } // lib.optionalAttrs (headroomMcpConfig.minContextTokens != null) {
      minContextTokens = headroomMcpConfig.minContextTokens;
    } // lib.optionalAttrs (headroomMcpConfig.minMessageChars != null) {
      minMessageChars = headroomMcpConfig.minMessageChars;
    };
    skills.customDirectories = [ "${homeDir}/.pi/agent/skills" ];
    # Auto-load tmux-agent-sidebar status extension for tmux-agent-sidebar integration.
    # The extension writes @pane_agent / @pane_status per pane so the sidebar
    # plugin can display OMP sessions alongside Claude Code and Codex.
    extensions = [ "${homeDir}/.omp/agent/extensions/tmux-agent-sidebar-status.ts" ];
    memory.backend = "hindsight";
    hindsight.mentalModelAutoSeed = true;
    compaction.thresholdPercent = 70;
    retry.fallbackChains.default = [
      "${piSettings.defaultProvider or "openai-codex"}/gpt-5.4"
    ];
  };
  finalOmpConfig = lib.recursiveUpdate defaults cfg.settings;

  # Build mcp.json attrs. No read of the deployed file; module owns the full
  # server set: built-in headroom/agentmemory/hindsight + cfg.extraMcpServers.
  mcpJson = {
    mcpServers = {
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
        args = [ "-y" "@agentmemory/agentmemory" ];
      };
      hindsight = {
        command = "npx";
        args = [ "-y" "hindsight-mcp" ];
      };
    } // cfg.extraMcpServers;
  };

  # pi-models.json is pi's native format (see migration inputs above), whose
  # apiKey syntax ($VAR / ${VAR} / !cmd / literal) differs from OMP's
  # (bare-VAR-name-or-literal / !cmd). Translate simple $VAR / ${VAR}
  # references to OMP's bare-name form when building models.yml; !cmd and
  # literals already work identically in both and pass through unchanged.
  # Does not handle mixed interpolation (e.g. "${FOO}_BAR") — pi-models.json
  # only uses whole-value $VAR references today.
  toOmpApiKey = v:
    if !(builtins.isString v) then v
    else if lib.hasPrefix "!" v then v
    else if lib.hasPrefix "$$" v then v
    else if lib.hasPrefix "\${" v && lib.hasSuffix "}" v
      then builtins.substring 2 (builtins.stringLength v - 3) v
    else if lib.hasPrefix "$" v
      then builtins.substring 1 (builtins.stringLength v - 1) v
    else v;

  ompModels = piModels // {
    providers = lib.mapAttrs
      (_: provider: provider // lib.optionalAttrs (provider ? apiKey) {
        apiKey = toOmpApiKey provider.apiKey;
      })
      (piModels.providers or {});
  };

  # Materialize managed files into the Nix store as regular files (not
  # symlinks). The activation step below copies them to the user's home.
  configYmlFile = pkgs.writeText "omp-config.yml" (builtins.toJSON finalOmpConfig);
  mcpJsonFile = pkgs.writeText "omp-mcp.json" (builtins.toJSON mcpJson);
  modelsYmlFile = pkgs.writeText "omp-models.yml" (lib.generators.toYAML {} ompModels);
  marketplacesJsonFile = pkgs.writeText "omp-marketplaces.json"
    (builtins.toJSON [{ url = ompMarketplaceUrl; type = "git"; }]);

in {
  options.omp = with lib; {
    enable = mkEnableOption "Oh My Pi (OMP) agent configuration";

    settings = mkOption {
      type = types.attrs;
      default = {};
      description = ''
        OMP config attrs to deep-merge on top of module defaults. Used to
        override module policy explicitly. Do NOT use this to migrate
        runtime edits from ~/.omp/agent/config.yml — declare intent in
        Nix instead.
      '';
    };

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
    # Materialize managed files as regular files (not symlinks), so OMP can
    # write flock(2) locks next to them. Runs after linkGeneration so any
    # prior symlinks are visible and replaceable.
    #
    # User-edited files are PRESERVED across deploys: if a managed file already
    # exists on disk, we leave it alone. To re-seed defaults from the module,
    # delete the file first. This avoids clobbering the user's runtime edits
    # (which is what OMP itself writes back to these files). The "by design"
    # overwrite behavior is gated behind an opt-in flag below.
    home.activation.ompMaterializeConfigs = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      mkdir -p "${ompAgentDir}" "${ompDir}"
      if [ ! -e "${ompAgentDir}/config.yml" ]; then
        ${pkgs.coreutils}/bin/install -m 600 "${configYmlFile}" "${ompAgentDir}/config.yml"
      fi
      if [ ! -e "${ompAgentDir}/models.yml" ]; then
        ${pkgs.coreutils}/bin/install -m 600 "${modelsYmlFile}" "${ompAgentDir}/models.yml"
      fi
      if [ ! -e "${ompAgentDir}/mcp.json" ]; then
        ${pkgs.coreutils}/bin/install -m 600 "${mcpJsonFile}" "${ompAgentDir}/mcp.json"
      fi
      if [ ! -e "${ompDir}/marketplaces.json" ]; then
        ${pkgs.coreutils}/bin/install -m 600 "${marketplacesJsonFile}" "${ompDir}/marketplaces.json"
      fi
    '';

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
      skillTarget="${ompAgentDir}/skills/ensemble"
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
