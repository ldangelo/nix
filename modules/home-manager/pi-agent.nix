{ pkgs, config, lib, ... }:

let
  cfg = config.pi-agent;
  piAgentHome = "${config.home.homeDirectory}/.pi/agent";
  binDir = "${piAgentHome}/bin";

  # Generate settings.json from options
  makeSettings = pkgs.lib.generators.toJSON {};

in {
  options.pi-agent = {
    enable = pkgs.lib.mkOption {
      type = pkgs.lib.types.bool;
      default = false;
      description = "Enable Pi Agent configuration";
    };

    settings = pkgs.lib.mkOption {
      type = pkgs.lib.types.attrs;
      default = {};
      description = "Settings for Pi Agent";
    };

    models = pkgs.lib.mkOption {
      type = pkgs.lib.types.attrs;
      default = {};
      description = "Models configuration for Pi Agent";
    };

    binTools = pkgs.lib.mkOption {
      type = pkgs.lib.types.listOf pkgs.lib.types.package;
      default = [];
      description = "Binary tools to include in pi-agent/bin directory";
    };

    packages = pkgs.lib.mkOption {
      type = pkgs.lib.types.listOf pkgs.lib.types.str;
      default = [];
      description = "Pi Agent packages to install (e.g., [\"npm:pi-powerline-footer\"])";
    };
  };

  config = lib.mkMerge [{
    home.packages = with pkgs; cfg.binTools;

    home.file.".pi/agent/settings.json" = {
      source = pkgs.writeText "pi-agent-settings.json" (makeSettings cfg.settings);
    };
  }];

  # Install pi-agent packages via activation script
  config = lib.mkIf (cfg.packages != []) {
    home.activation.installPiAgentPackages = let
      pkgInstallCommands = lib.concatMapStrings (pkg:
        "pi-agent package install ${pkg}\n"
      ) cfg.packages;
    in ''
      echo "Installing Pi Agent packages..."
      ${pkgInstallCommands}
      echo "Pi Agent packages installed successfully"
    '';
  };

  # Only create bin directory if there are tools to include
  config = lib.mkIf (cfg.binTools != []) {
    home.file.".pi/agent/bin" = {
      target = "${binDir}";
      recursive = true;
      executable = true;
      mode = "755";
      source = pkgs.runCommandLocal "pi-agent-bin" {
        buildInputs = cfg.binTools;
      } ''
        mkdir -p $out
        ${lib.concatMapStrings (tool: "cp ${tool}/bin/$(basename ${tool}) $out/\n") cfg.binTools}
      '';
    };
  };
}
