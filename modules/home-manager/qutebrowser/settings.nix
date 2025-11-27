# modules/home-manager/qutebrowser/settings.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.settings = {
    # UI Settings
    tabs.position = "left";
    tabs.width = "15%";
    tabs.show = "multiple";
    tabs.padding = { top = 4; bottom = 4; left = 8; right = 8; };
    tabs.indicator.width = 0;
    tabs.favicons.scale = 1.2;
    tabs.title.format = "{audio}{current_title}";
    statusbar.show = "in-mode";
    scrolling.smooth = true;
    window.title_format = "{perc}{current_title}";

    # Fonts
    fonts.default_family = "Source Code Pro";
    fonts.hints = "bold 18pt Source Code Pro";

    # Hints
    hints.radius = 3;
    hints.padding = { top = 2; bottom = 2; left = 4; right = 4; };
    hints.uppercase = true;
    hints.border = "none";

    # Privacy
    content.cookies.accept = "no-3rdparty";
    content.javascript.enabled = true;

    # Content
    colors.webpage.preferred_color_scheme = "dark";
    colors.webpage.darkmode.enabled = true;
    content.pdfjs = true;

    # Downloads
    downloads.location.directory = "~/Downloads";
    downloads.location.prompt = false;
    downloads.position = "bottom";

    # Completion
    completion.height = "30%";
    completion.scrollbar.width = 0;
  };

  # Per-domain settings
  programs.qutebrowser.extraConfig = ''
    # Per-domain cookie settings
    config.set('content.cookies.accept', 'all', '*.github.com')
    config.set('content.cookies.accept', 'no-3rdparty', '*.google.com')
    config.set('content.cookies.accept', 'no-3rdparty', '*.duckduckgo.com')
  '';
}
