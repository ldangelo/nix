{ config, lib, pkgs, namespace, ... }:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt;

  cfg = config.${namespace}.programs.terminal.tools.hammerspoon;
in {
  options.${namespace}.programs.terminal.tools.hammerspoon = {
    enable = mkBoolOpt false "Whether or not to enable hammerspoon";
  };

  config = mkIf cfg.enable {
    # setup hammerspoon directory
    home.file.".hammerspoon" = {
      source = lib.cleanSourceWith { src = lib.cleanSource ./config/.; };
      recursive = true;
    };

  };
}
