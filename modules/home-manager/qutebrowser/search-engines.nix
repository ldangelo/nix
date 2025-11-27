# modules/home-manager/qutebrowser/search-engines.nix
{ config, pkgs, lib, ... }:

{
  programs.qutebrowser.searchEngines = {
    DEFAULT = "https://duckduckgo.com/?q={}";
    g = "https://www.google.com/search?q={}";
    gh = "https://github.com/search?q={}";
    nix = "https://search.nixos.org/packages?query={}";
    nixopt = "https://search.nixos.org/options?query={}";
    hm = "https://home-manager-options.extranix.com/?query={}";
    w = "https://en.wikipedia.org/wiki/{}";
    yt = "https://www.youtube.com/results?search_query={}";
    r = "https://www.reddit.com/search?q={}";
    so = "https://stackoverflow.com/search?q={}";
    mdn = "https://developer.mozilla.org/en-US/search?q={}";
    rs = "https://doc.rust-lang.org/std/?search={}";
    py = "https://docs.python.org/3/search.html?q={}";
    npm = "https://www.npmjs.com/search?q={}";
    crates = "https://crates.io/search?q={}";
  };
}
