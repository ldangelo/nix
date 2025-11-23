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
        # Fonts
        fonts.hints = "bold 18pt Source Code Pro";
        fonts.default_family = "Source Code Pro";

        # Minimal UI
        tabs.show = "multiple";
        statusbar.show = "in-mode";
        scrolling.smooth = true;

        # Tree-style tabs (left side)
        tabs.position = "left";
        tabs.width = "15%";
        tabs.padding = { top = 4; bottom = 4; left = 8; right = 8; };
        tabs.indicator.width = 0;
        tabs.favicons.scale = 1.2;
        tabs.title.format = "{audio}{current_title}";

        # Hints polish
        hints.radius = 3;
        hints.padding = { top = 2; bottom = 2; left = 4; right = 4; };
        hints.uppercase = true;
        hints.border = "none";

        # Content
        colors.webpage.preferred_color_scheme = "dark";
        colors.webpage.darkmode.enabled = true;

        # Completion/downloads
        completion.height = "30%";
        completion.scrollbar.width = 0;
        downloads.position = "bottom";

        # Window
        window.title_format = "{perc}{current_title}";
      };
 };
  # User specified programs
  programs.dircolors.enable = true;
}
