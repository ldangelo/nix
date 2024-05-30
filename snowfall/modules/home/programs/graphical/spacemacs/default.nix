{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let
  my-emacs = pkgs.emacsMacport.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages
    (epkgs:
      with epkgs; [
        notmuch
        afew
        isync
        msmtp
        vterm
        multi-vterm
        pdf-tools
        treesit-grammars.with-all-grammars
      ]);
  cfg = config.${namespace}.apps.spacemacs;
in {
  options.${namespace}.apps.spacemacs = with types; {
    enable = mkBoolOpt false "Whether or not to enable iTerm2.";
  };

  config = mkIf cfg.enable {
    programs.emacs = {
      enable = true;
      package = my-emacs-with-packages;
    };

    home.packages = with pkgs; [
      emacs-all-the-icons-fonts
      (aspellWithDicts (d: [ d.en d.sv ]))
      ghostscript
      shellcheck
      # tetex # Does not build properly (2024-03-09)
      poppler
      #    mu
      wordnet
      nil
      nixfmt
      fasd
    ];
  };
}
