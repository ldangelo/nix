{ config, pkgs, inputs, ... }: {
  imports = [
    # Program configurations with native home-manager support
    ./git.nix           # Git configuration
    ./jujutsu.nix       # Jujutsu (jj) configuration
    ./nvim              # Neovim configuration
    ./wezterm.nix       # WezTerm configuration
    ./programs.nix      # Other program configs (fzf, direnv, starship)
    ./packages.nix      # Migrated from Brewfile

    # Dotfiles from chezmoi
    ./dotfiles-chezmoi.nix  # All chezmoi-managed dotfiles

    # Legacy modules (keeping for reference)
    ./dotfiles          # Old dotfiles module
    ./path.nix
    ./shell.nix
    ./user.nix
    ./alias.nix
    ./spacemacs.nix
    # ./spacevim.nix    # Disabled - conflicts with nvim config
    ./borders
    #     ./sketchybar
  ];


  # packages are just installed (no configuration applied)
  # programs are installed and configuration applied to dotfiles
  home = {
    username = "ldangelo";
    homeDirectory = "/Users/ldangelo";
    packages = with pkgs; [
      #       nixVersions.latest

      #       ./spacevim.nix
      # user selected packages
      pkgs.cyrus_sasl
      pkgs.cyrus-sasl-xoauth2
      #       pkgs.isync this package does not support oauth
      pkgs.devbox
      pkgs.devpod
      pkgs.helix
      pkgs.raycast
      pkgs.warp-terminal
      pkgs.lazygit
      pkgs.nerdfonts
      pkgs.source-code-pro
      pkgs.aldente
      pkgs.apparency
      #      pkgs.apple-sdk
      pkgs.binutils
      pkgs.dockutil
      pkgs.duti
      #      pkgs.macfuse
      pkgs.mas
      pkgs.mysides
      #      pkgs.openwith
      pkgs.shortcat
      pkgs.swiftdefaultapps
      #      pkgs.trash
      #      pkgs.utm
      #      pkgs.xcode

      #      pkgs.emacs-macport
      #    pkgs.yabai
      pkgs.discord
      # Fleek Bling
      pkgs.git
      pkgs.htop
      pkgs.github-cli
      pkgs.glab
      pkgs.gnupg
      pkgs.fzf
      pkgs.ripgrep
      pkgs.vscode
      pkgs.just
      pkgs.nerdfonts
      #       (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
      pkgs.silver-searcher
      pkgs.thefuck
      pkgs.jq
      pkgs.fasd
      pkgs.zoxide
      pkgs.eza
      pkgs.scc
      pkgs.duf
      pkgs.just
    ];
  };

  fonts.fontconfig.enable = true;

  # To figure this out (in-case it changes) you can comment out the line and see what version it expected.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself
  programs.home-manager.enable = true;
}
