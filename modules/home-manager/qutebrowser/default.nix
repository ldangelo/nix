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

  # Manage qutebrowser config only. Installing the Nix package pulls qtwebengine,
  # which is brittle on Darwin and currently hits bad Linux substitutes for ffmpeg/openapv.
  programs.qutebrowser.package = null;
}
