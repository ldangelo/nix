{ config, pkgs, ... }:


let my-emacs = pkgs.emacsMacport.override {
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
  programs.emacs = {
    enable = true;
    package = my-emacs-with-packages;
  };



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


  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    (aspellWithDicts (d: [d.en d.sv]))
    ghostscript
    shellcheck
    # tetex # Does not build properly (2024-03-09)
    poppler
    mu
    wordnet
    nil
    nixfmt-classic
  ];
}
