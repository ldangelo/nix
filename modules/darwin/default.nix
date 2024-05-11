{ nixpkgs, nix-darwin, ... }: {
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

  imports = [
    ./system.nix
    ./yabai/default.nix
    ./skhd/default.nix
    ./sketchybar/default.nix
  ];

  homebrew = {
    enable = true;

    taps = [
      "FelixKratz/formulae"
    ];


    masApps = {
      _1PasswordforSafari = 1569813296;
      userscripts = 1463298887;
      vimari = 1480933944;
    };

    brews = [
      "borders"
    ];

    casks = [
      "1password"
      "rider"
    ];
  };


  system.activationScripts.postUserActivation.text = ''
    # Following line should allow us to avoid a logout/login cycle
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
  '';
}
