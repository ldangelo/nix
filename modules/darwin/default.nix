{ pkgs, ... }: {
  # here go the darwin preferences and config items
  users.users.ldangelo.home = "/Users/ldangelo";
  #  system.defaults.dock.mru_space = false;  # do not rearrange spaces

  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  programs = {
    zsh.enable = true;
    gnupg.agent.enable = true;
  };

  fonts.fontDir.enable = true;
  fonts.fonts = [ pkgs.dejavu_fonts ];
  #  services.sketchybar.enable = true;
  #  services.sketchybar.package = pkgs.sketchybar;

  imports = [ ./system.nix ./yabai/default.nix ./skhd/default.nix ];

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

  system.activationScripts.postUserActivation.text = ''
    # Following line should allow us to avoid a logout/login cycle
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';
}
