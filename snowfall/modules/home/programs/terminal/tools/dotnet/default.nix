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

  cfg = config.${namespace}.programs.terminal.tools.dotnet;
in {
  options.${namespace}.programs.terminal.tools.dotnet = {
    enable = mkBoolOpt false "Whether or not to enable dotnet";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      dotnet-runtime
      omnisharp-roslyn
    ];
  };

}
