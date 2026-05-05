{ pkgs, config, ... }:

{
  imports = [
    ./git.nix
    ./jujutsu.nix
    ./nvim
    ./emacs
    ./wezterm.nix
    ./qutebrowser
    ./lazygit.nix
    ./programs.nix
    ./packages.nix
    ./dotfiles-chezmoi.nix
    ./dotfiles
    ./path.nix
    ./shell.nix
    ./user.nix
    ./alias.nix
    ./nix-search-tv.nix
    ./home.nix
    ./borders
    ./zoxide.nix
    ./tmux
    ./ghostty.nix
    ./yazi.nix
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
