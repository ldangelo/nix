;{ lib, pkgs, ... }: {
  environment.systemPackages = let
    spacevimRepo = pkgs.fetchgit {
      url = "https://github.com/SpaceVim/SpaceVim.git";
      rev = "9b354e05b4716b645ba6366e1265a5048a0c23d5";
      sha256 = "1mn28hf857kp0jmbgd89cf5mk4dg53jcbqqrbr9zi3b854sa9ads";
    };

    vimRc = pkgs.writeText "vimrc" ''
      " search/grep case insensitive
      :set ignorecase

      " tabs should always be 2 spaces
      set et ts=2 sts=2 sw=2

      " show Trailing Whitespaces
      :set list listchars=tab:»·,trail:¶

      " start spacevim
      source ${spacevimRepo}/init.vim

      " configure command cross
      "":hi CursorLine   cterm=NONE ctermbg=0 guibg=#073642
      "":hi CursorColumn cterm=NONE ctermbg=0 guibg=#073642
      ""set cursorline
      ""set cursorcolumn

      " disable noisy indentLine
      let g:indentLine_enabled = 0
    '';
  in [

    # vim
    (pkgs.writers.writeDashBin "spacevim" ''
      exec ${pkgs.neovim}/bin/nvim -u ${vimRc} "$@"
    '')
  ];
}
