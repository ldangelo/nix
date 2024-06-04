{ config, lib, pkgs, namespace, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  inherit (lib.${namespace}) enabled;

  cfg = config.${namespace}.programs.terminal.tools.home-manager;
in {
  options.${namespace}.programs.terminal.tools.home-manager = {
    enable = mkEnableOption "home-manager";
  };

  config = mkIf cfg.enable {
    
  stylix = {
    polarity = "dark";
    autoEnable = true;

    base16Scheme = "${pkgs.base16-schemes}/share/themes/darcula.yaml";
    fonts = {
      serif = { 
        package  = pkgs.source-code-pro;
        name = "Source Code Pro"; 
      };
      sansSerif = { 
        package  = pkgs.source-code-pro;
        name = "Source Code Pro"; 
      };
      monospace = { 
        package = pkgs.jetbrains-mono;
        name = "JetBrainsMonoNL Nerd Font Mono"; 
      };
      sizes = {
        terminal=16;
        desktop=14;
        applications=14;
      };
    };
  };
   
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
      pkgs.dejavu_fonts
      pkgs.source-code-pro
      pkgs.aldente
      pkgs.apparency
      #      pkgs.apple-sdk
      pkgs.binutils
      pkgs.dockutil
      pkgs.duti
      pkgs.lua5_4 # needed for sketcybar
      pkgs.macfuse-stubs
      pkgs.mas
      pkgs.mysides
      darwin.openwith
#      darwin.osx-cpu-temp
      darwin.iproute2mac
      pkgs.shortcat
      pkgs.swiftdefaultapps
      darwin.trash
      pkgs.utm
      #      pkgs.xcode

      #      pkgs.emacs-macport
      #    pkgs.yabai
      pkgs.discord
#      pkgs.git
      pkgs.htop
      pkgs.btop
#      pkgs.github-cli
#      pkgs.glab
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
};
}
