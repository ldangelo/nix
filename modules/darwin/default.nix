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

#   fonts.fonts = [ nixpkgs.source-code-pro ];
#  services.sketchybar.enable = true;
#  services.sketchybar.package = pkgs.sketchybar;

  imports = [
    ./system.nix
    #./yabai/default.nix
    #./skhd/default.nix
    ./homebrew.nix      # Migrated from Brewfile
  ];

  homebrew = {
    enable = true;

    taps = [
      "FelixKratz/formulae"
    ];


    masApps = {
#      _1PasswordforSafari = 1569813296;
#      userscripts = 1463298887;
#      vimlike = 1584519802;
#      vimari = 1480933944;
    };

    brews = [
      "podman"
      "docker-compose"
#       "isync" # the nix package doesn't support oauth
      "borders"
    ];

    casks = [
      "1password"
      "1password-cli"
      "claude-code"
      "devbox"
      "rider"
      "alt-tab"
      "wezterm"
    ];
  };


#  system.activationScripts.postUserActivation.text = ''
    # Following line should allow us to avoid a logout/login cycle
#    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
#  '';
}
