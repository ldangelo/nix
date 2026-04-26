{ pkgs, ... }: {
  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    # Source of truth: dotfiles/config/skhd/skhdrc — read at build time into /etc/skhdrc
    skhdConfig = builtins.readFile ../../../dotfiles/config/skhd/skhdrc;
  };
}
