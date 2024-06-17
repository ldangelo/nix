{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.programs.graphical.wezterm;
in {
  options.${namespace}.programs.graphical.wezterm = with types; {
    enable = mkBoolOpt false "Whether or not to enable wezterm.";
  };

  config = mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      enableZshIntegration = true;

      extraConfig = builtins.readFile config/wezterm.lua;
    };
  };
}

