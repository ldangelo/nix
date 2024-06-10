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

#      apps.doomemacs = enabled;

      #      apps.spacemacs = enabled;
#      spacevim is spewing too much crap into the terminal when deployed.
#      apps.spacevim = enabled;
    };
  };
}
