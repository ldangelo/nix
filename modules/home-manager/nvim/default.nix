{ ... }:

{
  programs.neovim = {
    enable = true;
    withRuby = true;    # keep legacy default (stateVersion < 26.05)
    withPython3 = true; # keep legacy default (stateVersion < 26.05)
  };

  home.file.".config/nvim" = {
    source = ../../../dotfiles/config/nvim;
    recursive = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
}
