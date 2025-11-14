{ config, pkgs, ...}:
{

  home.file.".SpaceVim" = {
    # don't make the directory read only so that impure melpa can still happen
    # for now
    recursive = true;
    source = pkgs.fetchFromGitHub {
      owner = "SpaceVim";
      repo = "SpaceVim";
      rev = "master";
      sha256 = "sha256-AtOSrVRKzswXzU/xDR7/AE98OenEBXSlr4hmQgTmPyo=";
    };
  };
  # install nvim
  home.packages = with pkgs; [
    neovim
  ];

  # link spacevim too nvim directory
  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink /Users/ldangelo/.SpaceVim; 
}
