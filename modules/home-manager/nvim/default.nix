{ ... }:

{
  programs.neovim.enable = true;
  # Silence deprecation warnings: stateVersion < 26.05 uses legacy defaults (true).
  # Set explicitly to keep legacy behavior going forward.
  programs.neovim.withRuby = true;
  programs.neovim.withPython3 = true;

  home.file.".config/nvim" = {
    source = ../../../dotfiles/config/nvim;
    recursive = true;
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    VISUAL = "nvim";
  };
}
