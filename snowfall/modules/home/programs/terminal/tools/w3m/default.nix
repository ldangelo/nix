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

  cfg = config.${namespace}.programs.terminal.tools.w3m;
in {
  options.${namespace}.programs.terminal.tools.w3m = {
    enable = mkBoolOpt false "Whether or not to enable w3m";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ w3m ];
  };

}
