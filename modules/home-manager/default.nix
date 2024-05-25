{ config, pkgs, inputs, ... }: {
  imports = [
    ./dotfiles
    #     ./nvim
    #     ./emacs
    ./programs.nix
    ./path.nix
    ./shell.nix
    ./user.nix
    ./alias.nix
    #     ./trash
    ./spacemacs.nix
    ./spacevim.nix
    ./borders
    #     ./sketchybar
  ];

  stylix.autoEnable = true;

  stylix.base16Scheme =
    "${pkgs.base16-schemes}/share/themes/darcula.yaml";
  stylix.fonts.sizes.terminal=16;
  stylix.fonts.sizes.desktop=14;
  stylix.fonts.sizes.applications=14;
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
      #       pkgs.warp-terminal
      pkgs.alacritty
      pkgs.kitty
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
      pkgs.btop
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
