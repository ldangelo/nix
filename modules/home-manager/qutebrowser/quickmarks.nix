# modules/home-manager/qutebrowser/quickmarks.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.quickmarks = {
    gh = "https://github.com";
    ghme = "https://github.com/ldangelo";
    nixpkgs = "https://github.com/NixOS/nixpkgs";
    hm = "https://github.com/nix-community/home-manager";
    hmopt = "https://home-manager-options.extranix.com/";
    nixsearch = "https://search.nixos.org/packages";
    claude = "https://claude.ai";
    hn = "https://news.ycombinator.com";
    reddit = "https://www.reddit.com";
    yt = "https://www.youtube.com";
    mail = "https://mail.google.com";
    cal = "https://calendar.google.com";
  };
}
