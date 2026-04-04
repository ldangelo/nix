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
        pager = "delta";
        diff-formatter = ":git";
      };

      merge-tools = {
        delta = {
          diff-invocation-mode = "file-by-file";
          diff-args = [ "$left" "$right" ];
          diff-expected-exit-codes = [ 0 1 ];
        };

        meld = {
          meld = "/opt/homebrew/bin/meld";
          edit-args = [ "--newtab" "$left" "$right" ];
        };
      };
    };
  };
}
