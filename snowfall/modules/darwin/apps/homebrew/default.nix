{ options, config, pkgs, lib, inputs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.home;
in {
  homebrew = {
    enable = true;
    onActivation.cleanup = "zap";

    taps = [ "FelixKratz/formulae" ];

    masApps = {
      _1PasswordforSafari = 1569813296;
      userscripts = 1463298887;
      vimlike = 1584519802;
      vimari = 1480933944;
    };

    brews = [
      "podman"
      "docker-compose"
      "borders"
      #       "isync" # the nix package doesn't support oauth
    ];

    casks = [ "1password" "1password-cli" "rider" "alt-tab" "zoom" ];
  };
}
