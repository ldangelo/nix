# modules/home-manager/qutebrowser/userscripts.nix
{ config, pkgs, lib, ... }:

{
  # Install userscripts to XDG data directory
  xdg.dataFile = {
    # Custom org-capture script
    "qutebrowser/userscripts/org-capture" = {
      source = ./scripts/org-capture;
      executable = true;
    };
  };

  # Ensure required dependencies are installed
  home.packages = with pkgs; [
    choose-gui  # Native macOS fuzzy finder for 1Password selection
    jq          # JSON parsing for 1Password
  ];

  # Set environment variable for qute-1pass to use choose
  home.sessionVariables = {
    QUTE_1PASS_DMENU_CMD = "choose";
  };
}
