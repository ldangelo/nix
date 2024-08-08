{ options, config, lib, pkgs, namespace, inputs, ... }:
with lib;
with lib.${namespace};
let
  #   my-emacs = pkgs.emacsMacport.override {
  #     withNativeCompilation = true;
  #     withSQLite3 = true;
  #     withTreeSitter = true;
  #     withWebP = true;
  #     withImageMagick = true;
  #     withMailutils = true;
  # #    withXwidgets = true;
  #   };
  #   Moved too an overlay
  my-emacs = pkgs.emacsNoctuidWithPackages;

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

    programs.emacs = {
      enable = true;
      #      doomPrivateDir = ./doom.d;
      package = my-emacs;
    };

    home.packages = with pkgs; [
      ## Emacs itself
      binutils # native-comp needs 'as', provided by this
      libtool
      cmake
      # :lang latex & :lang org (latex previews)
      texliveFull.out

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
      # :lang beancount
      beancount
      #fava # HACK Momentarily broken on nixos-unstable
      emacs-all-the-icons-fonts
      dotnet-runtime
      omnisharp-roslyn
      coreutils
      nixfmt
      nodejs
    ];

    xdg.configFile."emacs".source = builtins.fetchGit {
      url = "https://github.com/doomemacs/doomemacs.git";
      ref = "master";
      rev = "f5b3958331cebf66383bf22bdc8b61cd44eca645";
    };

    xdg.configFile."doom".source = ./doom.d;

    home.sessionVariables = {
      DOOMDIR = "${config.xdg.configHome}/doom";
      EMACSDIR = "${config.xdg.configHome}/emacs";
      DOOMLOCALDIR = "${config.xdg.dataHome}/doom";
      DOOMPROFILELOADFILE = "${config.xdg.stateHome}/doom-profiles-load.el";
    };
    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
  };

  #  options.programs.terminal.shells.zsh = {
  #    PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];
  #    rcFiles = [ "$XDG_CONFIG_CONFIG/emacs/aliases.zsh" ];
  #  };

}
