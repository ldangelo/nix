{ config, lib, pkgs, namespace, ... }:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt;

  cfg = config.${namespace}.programs.graphical.editors.emacs;
in {
  options.${namespace}.programs.graphical.editors.emacs = {
    enable = mkBoolOpt false "Whether or not to enable vscode.";
  };

  config = mkIf cfg.enable {
    programs.emacs.enable = true;

    home.file.".emacs.d" = {
      # don't make the directory read only so that impure melpa can still happen
      # for now
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "syl20bnr";
        repo = "spacemacs";
        rev = "develop";
        sha256 = "sha256-D5uI9nIf0Ocxs6ZPj9/BebFM81bizZdSAHRu43csuMA=";
      };
    };
  };
}
