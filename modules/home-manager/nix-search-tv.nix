{config, inputs, ...}:

{
  programs.nix-search-tv.enable = true;

  programs.zsh.shellAliases = {
      ns = "nix-search-tv print | fzf --preview='nix-search-tv preview {}' --scheme history";
    }
}
