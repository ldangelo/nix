{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace}; let
  cfg = config.${namespace}.apps.spacemacs;
in {
  options.${namespace}.apps.spacemacs = with types; {
    enable = mkBoolOpt false "Whether or not to enable iTerm2.";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      spacemacs
    ];
  };
}
