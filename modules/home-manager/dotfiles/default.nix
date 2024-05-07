{ pkgs, ... }: {
  home.packages = [
    pkgs.nix-tree
    pkgs.isync
  ];

  imports = [
    ./mbsync
  ];
}
