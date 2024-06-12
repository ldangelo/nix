{
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt;

  cfg = config.${namespace}.programs.terminal.tools.cmake;
in {
  options.${namespace}.programs.terminal.tools.cmake = {
    enable = mkBoolOpt false "Whether or not to enable cmake";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ cmake libtool ];
  };

}
