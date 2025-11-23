{ pkgs, misc, ... }: {
  programs.direnv.enable = true;
  programs.direnv.stdlib = ''
    : ''${XDG_CACHE_HOME:=$HOME/.cache}
    declare -A direnv_layout_dirs
    direnv_layout_dir() {
      echo "''${direnv_layout_dirs[$PWD]:=$(
        local hash="$(sha1sum - <<<"''${PWD}" | cut -c-7)"
        local path="''${PWD//[^a-zA-Z0-9]/-}"
        echo "''${XDG_CACHE_HOME}/direnv/layouts/''${hash}''${path}"
      )}"
    }
  '';
  programs.fzf = {
    enable = true;
    defaultOptions = [ "--bind ctrl-k:kill-line --color=dark" ];
  };

  programs.starship.enable = true;
  programs.starship.enableZshIntegration = true;

  programs.qutebrowser = {
      enable = true;
      settings = {
        fonts.keyhint = "15pt";
      }
  };
  # User specified programs
  programs.dircolors.enable = true;
}
