{ nixpkgs, nix-darwin, ... }: {
  # here go the darwin preferences and config items
  programs.zsh.enable = true;
  users.users.ldangelo.home = "/Users/ldangelo";
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  imports = [
    ./system.nix
    ./yabai/default.nix
    ./skhd/default.nix

    #    ./spacebar.nix
  ];

  homebrew = {
    enable = true;

    masApps = {
      _1PasswordforSafari = 1569813296;
      userscripts = 1463298887;
      vimari = 1480933944;
    };

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
