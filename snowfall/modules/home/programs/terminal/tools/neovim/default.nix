inputs@{ options, config, lib, pkgs, namespace, ... }:
with lib;
with lib.${namespace};
let cfg = config.${namespace}.cli-apps.neovim;
in {
  options.${namespace}.cli-apps.neovim = with types; {
    enable = mkBoolOpt false "Whether or not to enable neovim.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs;
      [
        # FIXME: As of today (2022-12-09), `page` no longer works with my Neovim
        # configuration. Either something in my configuration is breaking it or `page` is busted.
        # page
        neovim
      ];

    home.sessionVariables = {
      # PAGER = "page";
      # MANPAGER =
      #   "page -C -e 'au User PageDisconnect sleep 100m|%y p|enew! |bd! #|pu p|set ft=man'";
      PAGER = "less";
      MANPAGER = "less";
      NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      EDITOR = "nvim";
    };

    programs.zsh.shellAliases.vimdiff = "nvim -d";
    };
}
