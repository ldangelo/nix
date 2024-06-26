{ config, pkgs, ...}:
{

  home.file.".SpaceVim" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchgit {
      url = "https://spacevim.org/git/repos/SpaceVim/";
      hash = "sha256-0KSxrUa4yo65Om/rQ38nEQ/wpZCOBjjkcAmgr+Q91vI=";
    };
  };
  # install nvim
  home.packages = with pkgs; [
    neovim
  ];

  # link spacevim too nvim directory
  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink /Users/ldangelo/.SpaceVim; 
}
