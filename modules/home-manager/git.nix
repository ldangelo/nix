{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    userName = "Leo A. D'Angelo";
    userEmail = "ldangelo@mac.com";

    extraConfig = {
      core = {
        editor = "emacs";
        excludesfile = "${config.home.homeDirectory}/.gitignore_global";
        autocrlf = "input";
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
