{ config, pkgs, inputs, ... }: {
   imports = [
     ./dotfiles
     ./nvim
     ./emacs
     ./programs.nix
     ./path.nix
     ./shell.nix
     ./user.nix
     ./alias.nix
     #     ./trash
   ];

   nixpkgs = {
     overlays = [
       #       inputs.brew-nix.overlay.${builtins.currentSystem}
     ];
};


   # packages are just installed (no configuration applied)
   # programs are installed and configuration applied to dotfiles
   home = {
     username = "ldangelo";
     homeDirectory = "/Users/ldangelo";
     packages = with pkgs; [
       nixVersions.latest

       # user selected packages
       pkgs.helix
       pkgs.raycast
       pkgs.warp-terminal
       pkgs.lazygit
       pkgs.nerdfonts
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
       (pkgs.nerdfonts.override { fonts = [ "FiraCode" ]; })
       pkgs.silver-searcher
     ];
   };

   fonts.fontconfig.enable = true;
   home.stateVersion =
     "22.11"; # To figure this out (in-case it changes) you can comment out the line and see what version it expected.
   programs.home-manager.enable = true;
   }
