# modules/home-manager/qutebrowser/default.nix
{ config, pkgs, lib, ... }:

{
  imports = [
    ./settings.nix
    ./keybindings.nix
    ./search-engines.nix
    ./quickmarks.nix
    ./userscripts.nix
  ];

  # Enable qutebrowser
  programs.qutebrowser.enable = true;
}
