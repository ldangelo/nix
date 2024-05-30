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
  cfg = config.${namespace}.programs.terminal.tools.direnv;
in {
  options.${namespace}.programs.terminal.tools.direnv = with types; {
    enable = mkBoolOpt false "Whether or not to enable iTerm2.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      direnv
    ];
  };
}
