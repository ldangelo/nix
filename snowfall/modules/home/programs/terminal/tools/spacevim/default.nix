{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.apps.spacevim;
in {
  options.${namespace}.apps.spacevim = with types; {
    enable = mkBoolOpt false "Whether or not to enable iTerm2.";
  };

  config =
    mkIf cfg.enable { home.packages = with pkgs; [ spacevim ]; };
}
