{ config, pkgs, ... }:

{
  # Install neovim
  home.packages = [ pkgs.neovim ];

  # Copy the entire nvim configuration from nix repo
  xdg.configFile."nvim".source = ../../../dotfiles/config/nvim;
  
  # Set as default editor
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
}
