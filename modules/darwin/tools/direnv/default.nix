{
  lib,
  config,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.tools.direnv;
in {
  options.${namespace}.tools.direnv = {
    enable = mkEnableOption "Direnv";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      direnv
    ];
  };
}
