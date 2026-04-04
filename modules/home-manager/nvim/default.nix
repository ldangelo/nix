{ ... }:

{
  programs.neovim.enable = true;

  home.file.".config/nvim" = {
    source = ../../../dotfiles/config/nvim;
    recursive = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
}
