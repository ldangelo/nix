{ config, pkgs, ... }:

{
  programs.jujutsu = {
    enable = true;

    settings = {
      user = {
        name = "Leo D'Angelo";
        email = "ldangelo@mac.com";
      };

      ui = {
        editor = "nvim";
      };

      merge-tools = {
        meld = {
          meld = "/opt/homebrew/bin/meld";
          edit-args = [ "--newtab" "$left" "$right" ];
        };
      };
    };
  };
}
