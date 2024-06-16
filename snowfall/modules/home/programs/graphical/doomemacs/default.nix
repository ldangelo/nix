{ options, config, lib, pkgs, namespace, inputs, ... }:
with lib;
with lib.${namespace};
let
  my-emacs = pkgs.emacsMacport.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
    withImageMagick = true;
    withMailutils = true;
#    withXwidgets = true;
  };

  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages
    (epkgs:
      with epkgs; [
        vterm
        multi-vterm
        pdf-tools
        treesit-grammars.with-all-grammars
      ]);

  cfg = config.${namespace}.apps.doomemacs;
in {
  options.${namespace}.apps.doomemacs = with types; {
    enable = mkBoolOpt false "Whether or not to enable doomemacs.";
    doom = rec {
      forgeUrl = mkOpt types.str "https://github.com";
      repoUrl = mkOpt types.str "${forgeUrl}/doomemacs/doomemacs";
      configRepoUrl =
        mkOpt types.string "${forgeUrl}/ldangelo/doom-emacs-private";
    };
  };

  config = mkIf cfg.enable {
#    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    programs.doom-emacs = {
      enable = true;
      doomPrivateDir = ./doom.d;
      package = my-emacs-with-packages;
    };


    home.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this
      libtool
      cmake
      # 28.2 + native-comp
      ((emacsPackagesFor emacsNativeComp).emacsWithPackages
        (epkgs: [ epkgs.vterm ]))

      ## Doom dependencies
      #      git
      (ripgrep.override { withPCRE2 = true; })
      gnutls # for TLS connectivity

      ## Optional dependencies
      fd # faster projectile indexing
      imagemagick # for image-dired
      pinentry-emacs # # in-emacs gnupg prompts
      zstd # for undo-fu-session/undo-tree compression

      ## Module dependencies
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :tools editorconfig
      editorconfig-core-c # per-project style config
      # :tools lookup & :lang org +roam
      sqlite
      # :lang latex & :lang org (latex previews)
      texlive.combined.scheme-medium
      # :lang beancount
      beancount
      fava # HACK Momentarily broken on nixos-unstable
      emacs-all-the-icons-fonts
      dotnet-runtime
      omnisharp-roslyn
      coreutils
      nixfmt
    ];

  };

#  options.programs.terminal.shells.zsh = {
#    PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
#    rcFiles = [ "$XDG_CONFIG_CONFIG/emacs/aliases.zsh" ];
#  };

}
