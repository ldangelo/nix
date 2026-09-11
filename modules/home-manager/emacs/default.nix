{
  config,
  pkgs,
  lib,
  ...
}:

let
  my-emacs = pkgs.emacs-macport.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };

  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (
    epkgs: with epkgs; [
      mu4e
      vterm
      multi-vterm
      pdf-tools
      treesit-grammars.with-all-grammars
    ]
  );
in
{
  # home.file.".config/emacs" = {
  #     source=../../../dotfiles/config/emacs;
  #     recursive=true;
  # };

  home.sessionVariables = {
    EMACS = "/Applications/Emacs.app/Contents/MacOS/Emacs";
  };

  # Doom's bin/doom launcher needs the Xcode/CommandLineTools toolchain and
  # a C locale on PATH before Oh-My-Zsh plugins initialize; keeping this
  # here (rather than hand-patched into ~/.config/emacs/bin/doom, which
  # `doom upgrade` resets) lets `doom upgrade` run without local diffs.
  programs.zsh.envExtra = lib.mkAfter ''
    export PATH="/usr/bin:/bin:/usr/sbin:/sbin:/Applications/Xcode.app/Contents/Developer/usr/bin:/Library/Developer/CommandLineTools/usr/bin:$PATH"
    # Ensure home-manager tools (e.g., git) take priority over system versions
    export PATH="/etc/profiles/per-user/$USER/bin:$PATH"
    # Scope C locale to the doom CLI wrapper so OMP (and other UTF-8
    # consumers, e.g. status-line `preset: nerd` Nerd Font glyphs) keep
    # multibyte handling outside of doom invocations.
    doom() { LANG=C LC_ALL=C command doom "$@"; }
  '';
  #
  # Doom is intentionally managed outside Nix.
  # The standalone configuration lives at ~/.config/doom.
  # home.file.".config/doom" = {
  #   source=../../../dotfiles/config/doom;
  #   recursive=true;
  # };

}
