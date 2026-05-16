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

  # On Darwin, keep managing qutebrowser config files but do not install the
  # Nix package: it pulls qtwebengine, which currently fails to build locally.
  programs.qutebrowser.package =
    lib.mkIf pkgs.stdenv.hostPlatform.isDarwin null;
}
