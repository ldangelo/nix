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

  cfg = config.${namespace}.programs.terminal.tools.rust;
in {
  options.${namespace}.programs.terminal.tools.rust = {
    enable = mkBoolOpt false "Whether or not to enable rust";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cargo
      rustc
      rustfmt
    ];
  };

}
