{ config, lib, pkgs, namespace, ... }:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt enabled;

  cfg = config.${namespace}.suites.development;
in {
  options.${namespace}.suites.development = {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };
  config = mkIf cfg.enable {

    oftheangels = {
 
  apps.spacemacs = enabled;
  apps.spacevim = enabled;
  };
  };
}

