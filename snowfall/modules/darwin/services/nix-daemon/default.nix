{ lib, config, namespace, ... }:
let
  inherit (lib) types mkIf;
  inherit (lib.${namespace}) mkOpt enabled;

  cfg = config.${namespace}.services.nix-daemon;
in {
  options.${namespace}.services.nix-daemon = {
    enable = mkOpt types.bool true "Whether to enable the Nix daemon.";
  };

  config = mkIf cfg.enable {
    services.nix-daemon = enabled;
    services.emacs-daemon = enabled; # run emacs as a daemon
    services.pizauth-daemon = {
      enable = true;
      homeDir = "/Users/ldangelo";
      cacheDir = "/Users/ldangelo/.local/share";
      logFile = "/tmp/pizauth.out";
    };
  };
}
