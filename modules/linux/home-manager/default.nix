{ pkgs, config, ... }:

{
  imports = [
    ../../home-manager/git.nix
    ../../home-manager/jujutsu.nix
    ../../home-manager/nvim
    ../../home-manager/emacs
    ../../home-manager/wezterm.nix
    ../../home-manager/qutebrowser
    ../../home-manager/lazygit.nix
    ../../home-manager/programs.nix
    ../../home-manager/packages.nix
    ../../home-manager/dotfiles-chezmoi.nix
    ../../home-manager/dotfiles
    ../../home-manager/path.nix
    ../../home-manager/shell.nix
    ../../home-manager/user.nix
    ../../home-manager/alias.nix
    ../../home-manager/nix-search-tv.nix
    ../../home-manager/home.nix
    ../../home-manager/zoxide.nix
    ../../home-manager/tmux
    ../../home-manager/yazi.nix
  ];

  home = {
    username = "ldangelo";
    homeDirectory = "/home/ldangelo";
  };

  catppuccin = {
    enable = true;
    atuin.enable = true;
    bat.enable = true;
    btop.enable = true;
    chromium.enable = true;
    fzf.enable = true;
    lazygit.enable = true;
    fish.enable = true;
    nushell.enable = true;
    starship.enable = true;
    nvim.enable = true;
    qutebrowser.enable = true;
    tmux.enable = true;
    yazi.enable = true;
  };

  fonts.fontconfig.enable = true;
  home.stateVersion = "22.11";

  programs.home-manager.enable = true;
}
