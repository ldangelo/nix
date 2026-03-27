{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    signing.format = "openpgp"; # silence home-manager warning (stateVersion < 25.05)

    settings = {
      user = {
        name = "Leo A. D'Angelo";
        email = "ldangelo@mac.com";
      };

      core = {
        editor = "nvim";
        excludesfile = "${config.home.homeDirectory}/.gitignore_global";
        autocrlf = "input";
        pager = "delta";
      };

      interactive = {
        diffFilter = "delta --color-only";
      };

      delta = {
        navigate = true;
        side-by-side = true;
        line-numbers = true;
        syntax-theme = "Catppuccin Mocha";
        minus-style = "syntax #3B1219";
        minus-emph-style = "syntax #6B2028";
        plus-style = "syntax #1B2B20";
        plus-emph-style = "syntax #2B4830";
        map-styles = "bold purple => syntax #330033, bold cyan => syntax #003333";
        line-numbers-minus-style = "#F38BA8";
        line-numbers-plus-style = "#A6E3A1";
        line-numbers-zero-style = "#6C7086";
      };

      merge = {
        conflictstyle = "zdiff3";
      };

      difftool = {
        sourcetree = {
          cmd = "opendiff \"$LOCAL\" \"$REMOTE\"";
          path = "";
        };
      };

      mergetool = {
        sourcetree = {
          cmd = "${config.home.homeDirectory}/Applications/Sourcetree.app/Contents/Resources/opendiff-w.sh \"$LOCAL\" \"$REMOTE\" -ancestor \"$BASE\" -merge \"$MERGED\"";
          trustExitCode = true;
        };
      };

      credential = {
        "https://github.com" = {
          helper = "!/opt/homebrew/bin/gh auth git-credential";
        };
        "https://gist.github.com" = {
          helper = "!/opt/homebrew/bin/gh auth git-credential";
        };
      };
    };
  };

  # Global gitignore
  home.file.".gitignore_global".text = ''
    # macOS
    .DS_Store
    .AppleDouble
    .LSOverride

    # Thumbnails
    ._*

    # Files that might appear in the root of a volume
    .DocumentRevisions-V100
    .fseventsd
    .Spotlight-V100
    .TemporaryItems
    .Trashes
    .VolumeIcon.icns
    .com.apple.timemachine.donotpresent

    # Directories potentially created on remote AFP share
    .AppleDB
    .AppleDesktop
    Network Trash Folder
    Temporary Items
    .apdisk

    # IDE
    .idea/
    .vscode/
    *.swp
    *.swo
    *~

    # Nix
    result
    result-*
  '';
}
