{ config, lib, pkgs, namespace, ... }:
let
  inherit (lib) mkIf;
  inherit (lib.${namespace}) mkBoolOpt enabled;

  cfg = config.${namespace}.suites.common;
in {
  options.${namespace}.suites.common = {
    enable = mkBoolOpt false "Whether or not to enable common configuration.";
  };

  config = mkIf cfg.enable {
    home.shellAliases = { nixcfg = "nvim ~/${namespace}/flake.nix"; };

    oftheangels = {
      programs = {
        graphical = {
          alacritty = enabled;
        };

        terminal = {
          tools = {
            direnv = enabled;
            git = enabled;
            home-manager = enabled;
            tmux = enabled;
            w3m = enabled;
            cmake = enabled;
          };

          shells = {
            zsh = enabled;
            fish = enabled;
          };
        };
      };
    };
  };
}
