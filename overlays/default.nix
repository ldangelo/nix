{ config, pkgs, lib, ...}:
{
  nixpkgs.overlays = [
    # sketchybar-lua overlay - wraps the package properly
    (final: prev: {
      sketchybar-lua = prev.callPackage ./sketchybar-lua { 
        sources = { sketchybar-lua = prev.fetchFromGitHub {
          owner = "FelixKratz";
          repo = "SbarLua";
          rev = "main";  # or pin to a specific commit
          sha256 = lib.fakeSha256;  # Will need to be updated on first build
        }; };
      };
    })
  ];
}
