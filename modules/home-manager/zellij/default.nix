{ config, pkgs, ... }:

{
  xdg.configFile."zellij/config.kdl".source = ./config.kdl;
  xdg.configFile."zellij/layouts".source = ./layouts;
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
}
