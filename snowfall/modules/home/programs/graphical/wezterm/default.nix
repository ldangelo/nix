{
  options,
  config,
  lib,
  pkgs,
  namespace,
  ...
}:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.programs.graphical.wezterm;
in {
  options.${namespace}.programs.graphical.wezterm = with types; {
    enable = mkBoolOpt false "Whether or not to enable wezterm.";
  };

  programs.wezterm = {
      enable = true;
      enableZshIntegration = true;
      enableFishIntegration = true;

      extraConfig = builtins.readFile config/wezterm.lua;
  }
}

