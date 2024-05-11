{ pkgs, ... }: {

  system = {
    keyboard.enableKeyMapping = true;
    keyboard.remapCapsLockToEscape = true;

    defaults = {
      NSGlobalDomain = {
        # Dark mode
        AppleInterfaceStyle = "Dark";

        # Show all file extensions
        AppleShowAllExtensions = true;

        # Automatically hide and show the menu bar
        _HIHideMenuBar = false;
      };

      dock = {
        # Automatically hide and show the Dock
        autohide = true;
        autohide-delay = 0.0;
        tilesize=48;
        launchanim=false;
        static-only=false;
        showhidden=true;
        show-recents=false;
        show-process-indicators=true;

        # Style options
        orientation = "bottom";
      };

      finder = {
        AppleShowAllExtensions = true;
        _FXShowPosixPathInTitle = true;
      };

      # Tab between form controls and F-row that behaves as F1-F12
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;
        "com.apple.keyboard.fnState" = true;
      };
    };
  };

  services.nix-daemon.enable = true;

  # backwards compat; don't change
  system.stateVersion = 4;
}
