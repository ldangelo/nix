{ config, pkgs, ... }:

let
  # Base path for all dotfiles in the nix repo
  dotfilesPath = ../../dotfiles;
in
{
  # Migrate all dotfiles from nix repo

  # .config directories
  xdg.configFile = {
    # Window managers
    "aerospace".source = "${dotfilesPath}/config/aerospace";
    "yabai".source = "${dotfilesPath}/config/yabai";
    "skhd".source = "${dotfilesPath}/config/skhd";

    # UI customization
    # borders - already handled by ./borders module

    # Keyboard
    "kanata".source = "${dotfilesPath}/config/kanata";

    # Emacs
    "doom".source = "${dotfilesPath}/config/doom";

    # Email
    "neomutt".source = "${dotfilesPath}/config/neomutt";
    "msmtp".source = "${dotfilesPath}/config/msmtp";

    # Shell
    "zsh".source = "${dotfilesPath}/config/zsh";

    # direnv - already handled by programs.direnv in programs.nix

    # Other tools
    "userscripts".source = "${dotfilesPath}/config/userscripts";
  };

  # Top-level dotfiles
  home.file = {
    ".emacs-profile".source = "${dotfilesPath}/emacs-profile";
    ".emacs-profiles.el".source = "${dotfilesPath}/emacs-profiles.el";
    ".ideavimrc".source = "${dotfilesPath}/ideavimrc";
    ".mailrc".source = "${dotfilesPath}/mailrc";
    ".mbsyncrc".source = "${dotfilesPath}/mbsyncrc";

    # Oh My Zsh
    ".oh-my-zsh".source = "${dotfilesPath}/oh-my-zsh";
  };
}
