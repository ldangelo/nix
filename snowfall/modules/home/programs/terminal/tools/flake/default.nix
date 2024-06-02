{ lib, config, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.programs.terminal.tools.flake;
in {
  options.${namespace}.programs.terminal.tools.flake = {
    enable = mkEnableOption "Flake";
  };

  config =
    mkIf cfg.enable { home.packages = with pkgs; [ snowfallorg.flake ]; };
}
