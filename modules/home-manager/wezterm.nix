{ config, pkgs, ... }:

{
  # WezTerm is installed via homebrew cask, but we manage the config via nix
  xdg.configFile."wezterm".source = ../../dotfiles/config/wezterm;
}
