
{ config, pkgs, ... }:


let my-emacs = pkgs.emacs-macport.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (epkgs: with epkgs; [
    mu4e
    vterm
    multi-vterm
    pdf-tools
    treesit-grammars.with-all-grammars
  ]
  );
in
{
  home.file.".config/emacs" = {
      source=../../../dotfiles/config/emacs;
      recursive=true;
  };

  home.file.".config/doom" = {
    source=../../../dotfiles/config/doom;
    recursive=true;
  };

}
