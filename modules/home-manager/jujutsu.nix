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
        diff-tool = "delta";
        diff-formatter = ":git";
      };

      lazyjj = {
        diff-tool = "delta";
      };

      merge-tools = {
        delta = {
          diff-args = [ "$left" "$right" "--side-by-side" "--line-numbers" ];
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
