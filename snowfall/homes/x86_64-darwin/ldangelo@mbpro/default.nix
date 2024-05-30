{ lib, pkgs, config, osConfig ? { }, format ? "unknown", namespace, ... }:
with lib.${namespace}; {
  oftheangels = {
    user = {
      enable = true;
      name = config.oftheangels.user.name;
    };

    cli-apps = {
#      neovim = enabled;
    };

    programs = {
      terminal = {
        shells = {
#          zsh = enabled;
        };

        tools = {
#          tmux = enabled;
#          git = enabled;
#          direnv = enabled;
#          home-manager = enabled;
        };
      };
    };
  };

  home.sessionPath = [ "$HOME/bin" ];

  home.stateVersion = "22.11";
}
