{
  pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4fe8d07066f6ea82cda2b0c9ae7aee59b2d241b3.tar.gz";
    sha256 = "sha256:06jzngg5jm1f81sc4xfskvvgjy5bblz51xpl788mnps1wrkykfhp";
  }) {}
}:
pkgs.stdenv.mkDerivation rec {
  pname = "spacevim";
  version = "1.0.0";

  spacevimRepo = pkgs.fetchgit {
    url = "https://github.com/SpaceVim/SpaceVim.git";
    rev = "9b354e05b4716b645ba6366e1265a5048a0c23d5";
    sha256 = "1mn28hf857kp0jmbgd89cf5mk4dg53jcbqqrbr9zi3b854sa9ads";
  };

  # vimRc = pkgs.writeText "vimrc" ''
  #     " search/grep case insensitive
  #     :set ignorecase

  #     " tabs should always be 2 spaces
  #     set et ts=2 sts=2 sw=2

  #     " show Trailing Whitespaces
  #     :set list listchars=tab:»·,trail:¶

  #     " start spacevim
  #     source ${spacevimRepo}/init.vim

  #     " configure command cross
  #     "":hi CursorLine   cterm=NONE ctermbg=0 guibg=#073642
  #     "":hi CursorColumn cterm=NONE ctermbg=0 guibg=#073642
  #     ""set cursorline
  #     ""set cursorcolumn

  #     " disable noisy indentLine
  #     let g:indentLine_enabled = 0
  #   '';
  # in [

  #   # vim
  #   (pkgs.writers.writeDashBin "spacevim" ''
  #     exec ${pkgs.neovim}/bin/nvim -u ${vimRc} "$@"
  #   '')
  # ];
  meta = with pkgs.lib; {
    description = "Spacevim";
    license = licenses.free;
    platforms = platforms.unix;
  };
}
