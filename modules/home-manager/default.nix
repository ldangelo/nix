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

   nixpkgs = {
     overlays = [
       #       inputs.brew-nix.overlay.${builtins.currentSystem}
     inputs.nixpkgs.isync.override { withCyrusSaslXoauth2 = true; }
     ];
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
