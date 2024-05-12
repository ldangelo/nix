{ config, pkgs, lib, ...}:
{
  nixpkgs.overlays = [
    ./sketchybar-lua
  ];
}
