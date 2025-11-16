{ config, pkgs, ... }:

{
  # Install neovim
  home.packages = [ pkgs.neovim ];

  # Copy the entire nvim configuration from nix repo
  home.file.".config/nvim" = {
      source=../../../dotfiles/config/nvim;
      recursive=true;
  };
  
  # Set as default editor
  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
}
